module StatsTest (statsTests) where

import Control.Logic
import DB
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import IPAddress
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import MinutesTime

import Control.Monad
import Test.Framework
import Stats.Control
import Stats.Model
import TestKontra

import qualified Log

import Control.Applicative
import Data.Ix
import Util.HasSomeUserInfo
import Data.Maybe
import Util.Actor

statsTests :: TestEnvSt -> Test
statsTests env = testGroup "Stats" [
    testThat "test invite stat"                  env testInviteStat
  , testThat "test receive stat"                 env testReceiveStat
  , testThat "test open stat"                    env testOpenStat
  , testThat "test link stat"                    env testLinkStat
  , testThat "test sign stat"                    env testSignStat
  , testThat "test reject stat"                  env testRejectStat
  , testThat "test delete stat"                  env testDeleteStat
  , testThat "test purge stat"                   env testPurgeStat
  , testThat "test time efficiency of userstats" env testGetUsersAndStats
  ]

testInviteStat :: TestEnv ()
testInviteStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                          isPreparation &&^
                                                          (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (systemActor time)
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatInviteEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual ("Should be Invite stat but was " ++ show (ssQuantity s)) SignStatInvite (ssQuantity s))

testReceiveStat :: TestEnv ()
testReceiveStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isPreparation &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ SetInvitationDeliveryStatus (documentid doc') (signatorylinkid sl) Delivered (systemActor time))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatReceiveEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatReceive (ssQuantity s))

testOpenStat :: TestEnv ()
testOpenStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isPreparation &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  _ <- forM (documentsignatorylinks doc')
       (\sl -> if isAuthor sl
               then dbUpdate $ MarkInvitationRead (documentid doc') (signatorylinkid sl)
                    (authorActor time noIP (userid author) (getEmail author))
               else dbUpdate $ MarkInvitationRead (documentid doc') (signatorylinkid sl)
                    (signatoryActor time noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatOpenEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatOpen (ssQuantity s))

testLinkStat :: TestEnv ()
testLinkStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isPreparation &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  _ <- forM (documentsignatorylinks doc')
       (\sl -> if isAuthor sl
               then dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl)
                    (authorActor time noIP (fromJust $ maybesignatory sl) (getEmail sl))
               else dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl)
                    (signatoryActor time noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatLinkEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatLink (ssQuantity s))

testSignStat :: TestEnv ()
testSignStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isPreparation &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  _ <- forM (documentsignatorylinks doc')
       (\sl -> if isAuthor sl
               then dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl)
                    (authorActor time noIP (fromJust $ maybesignatory sl) (getEmail sl))
               else dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl)
                    (signatoryActor time noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl)))
  _ <- forM (filter isSignatory $ documentsignatorylinks doc') (\sl -> dbUpdate $ SignDocument (documentid doc') (signatorylinkid sl) (signatorymagichash sl) Nothing (signatoryActor time noIP (maybesignatory sl) (getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatSignEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ filter isSignatory $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatSign (ssQuantity s))

testRejectStat :: TestEnv ()
testRejectStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isPreparation &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') (systemActor time)
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl' = getAuthorSigLink doc
      aa = authorActor time noIP (userid author) (getEmail author)
  _ <- dbUpdate $ RejectDocument (documentid doc)
         (signatorylinkid asl')
         Nothing
         aa
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  let Just asl = getAuthorSigLink d
  _ <- addSignStatRejectEvent d asl
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatReject (ssQuantity s))

testDeleteStat :: TestEnv ()
testDeleteStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isClosed &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  let actor = systemActor time
  _ <- dbUpdate $ ArchiveDocument author' (documentid doc') actor

  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl = getAuthorSigLink doc
  _ <- addSignStatDeleteEvent doc asl time
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatDelete (ssQuantity s))

testPurgeStat :: TestEnv ()
testPurgeStat = do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  author <- addNewRandomUser
  time <- getMinutesTime
  let actor = systemActor time
  _ <- dbUpdate $ SetUserCompany (userid author) Nothing
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isClosed &&^
                                                           (any isSignatory . documentsignatorylinks))
  _ <- dbUpdate $ ArchiveDocument author' (documentid doc') actor
  _ <- dbUpdate $ ReallyDeleteDocument author' (documentid doc') actor
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl = getAuthorSigLink doc
  _ <- addSignStatPurgeEvent doc asl time
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatPurge (ssQuantity s))

-- tests for old stats

testGetUsersAndStats :: TestEnv ()
testGetUsersAndStats = do
  time <- getMinutesTime
  us <- catMaybes <$> (forM (range (0, 100::Int)) $ \i->
                        dbUpdate $ AddUser ("F", "L") ("e" ++ show i ++ "@yoyo.com")
                        Nothing False Nothing Nothing (mkLocale REGION_SE LANG_SE))
  _ <- forM [(u, i) | u <- us, i <- range (0, 10::Int)] $ \(u,_) -> do
    let aa = authorActor time noIP (userid u) (getEmail u)
    dbUpdate $ NewDocument u Nothing "doc!" (Signable Contract) 0 aa

  Log.debug $ "Set up test, now running query."
  t0 <- getMinutesTime
  stats <- dbQuery $ GetUsersAndStats []
  Log.debug $ "Number of stats returned: " ++ show (length stats)
  t1 <- getMinutesTime

  let td = toSeconds t1 - toSeconds t0
  assertBool ("Should take less than one second, but took " ++ show td ++ " seconds.") $ td <= 1
