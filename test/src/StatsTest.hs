{-# OPTIONS_GHC -fno-warn-orphans #-}
module StatsTest (statsTests) where

import DB.Classes
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import MinutesTime
import Test.HUnit.Base (Assertion)

import DB.Nexus
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Stats.Control
import Stats.Model
import StateHelper

import qualified Data.ByteString.UTF8 as BS

import qualified Log

import Control.Applicative
import Data.Ix
import Util.HasSomeUserInfo
import Data.Maybe
import EvidenceLog.Model

statsTests :: Nexus -> Test
statsTests conn = testGroup "Stats" 
  [
    testCase "test invite stat"                  $ testInviteStat       conn                        
  , testCase "test receive stat"                 $ testReceiveStat      conn                      
  , testCase "test open stat"                    $ testOpenStat         conn                            
  , testCase "test link stat"                    $ testLinkStat         conn                            
  , testCase "test sign stat"                    $ testSignStat         conn                            
  , testCase "test reject stat"                  $ testRejectStat       conn                        
  , testCase "test delete stat"                  $ testDeleteStat       conn                        
  , testCase "test purge stat"                   $ testPurgeStat        conn                          
  , testCase "test time efficiency of userstats" $ testGetUsersAndStats conn 
  ]

testInviteStat :: Nexus -> Assertion
testInviteStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (SystemActor time)
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatInviteEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual ("Should be Invite stat but was " ++ show (ssQuantity s)) SignStatInvite (ssQuantity s))

testReceiveStat :: Nexus -> Assertion
testReceiveStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ SetInvitationDeliveryStatus (documentid doc') (signatorylinkid sl) Delivered (SystemActor time))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatReceiveEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatReceive (ssQuantity s))

testOpenStat :: Nexus -> Assertion
testOpenStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  _ <- forM (documentsignatorylinks doc') 
       (\sl -> if isAuthor sl 
               then dbUpdate $ MarkInvitationRead (documentid doc') (signatorylinkid sl)
                    (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))
               else dbUpdate $ MarkInvitationRead (documentid doc') (signatorylinkid sl)
                    (SignatoryActor time (IPAddress 0) (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatOpenEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatOpen (ssQuantity s))

testLinkStat :: Nexus -> Assertion
testLinkStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  _ <- forM (documentsignatorylinks doc') 
       (\sl -> if isAuthor sl 
               then dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) 
                    (AuthorActor time (IPAddress 0) (fromJust $ maybesignatory sl) (BS.toString $ getEmail sl))
               else dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) 
                    (SignatoryActor time (IPAddress 0) (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatLinkEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatLink (ssQuantity s))

testSignStat :: Nexus -> Assertion
testSignStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  _ <- forM (documentsignatorylinks doc') 
       (\sl -> if isAuthor sl 
               then dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) 
                    (AuthorActor time (IPAddress 0) (fromJust $ maybesignatory sl) (BS.toString $ getEmail sl))
               else dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) 
                    (SignatoryActor time (IPAddress 0) (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)))
  _ <- forM (filter isSignatory $ documentsignatorylinks doc') (\sl -> dbUpdate $ SignDocument (documentid doc') (signatorylinkid sl) (signatorymagichash sl) Nothing (SignatoryActor time (IPAddress 0) (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatSignEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ filter isSignatory $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatSign (ssQuantity s))

testRejectStat :: Nexus -> Assertion
testRejectStat conn = withTestEnvironment conn $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') (SystemActor time)
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl' = getAuthorSigLink doc
      aa = AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author)
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

testDeleteStat :: Nexus -> Assertion
testDeleteStat conn = withTestEnvironment conn $ do
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
  let actor = SystemActor time
  _ <- dbUpdate $ ArchiveDocument author' (documentid doc') actor

  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl = getAuthorSigLink doc
  _ <- addSignStatDeleteEvent doc asl time
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatDelete (ssQuantity s))

testPurgeStat :: Nexus -> Assertion
testPurgeStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  author <- addNewRandomUser
  time <- getMinutesTime
  let actor = SystemActor time
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

testGetUsersAndStats :: Nexus -> Assertion
testGetUsersAndStats conn = withTestEnvironment conn $ do
  time <- getMinutesTime
  us <- catMaybes <$> (forM (range (0, 100::Int)) $ \i->
                        dbUpdate $ AddUser (BS.fromString "F", BS.fromString "L") (BS.fromString $ "e" ++ show i ++ "@yoyo.com")
                        Nothing False Nothing Nothing (mkLocale REGION_SE LANG_SE))
  _ <- forM [(u, i) | u <- us, i <- range (0, 10::Int)] $ \(u,_) -> do
    let aa = AuthorActor time (IPAddress 0) (userid u) (BS.toString $ getEmail u)
    dbUpdate $ NewDocument u Nothing (BS.fromString "doc!") (Signable Contract) aa
  
  Log.debug $ "Set up test, now running query."
  t0 <- getMinutesTime
  stats <- dbQuery $ GetUsersAndStats
  Log.debug $ "Number of stats returned: " ++ show (length stats)
  t1 <- getMinutesTime
  
  let td = toSeconds t1 - toSeconds t0
  assertBool ("Should take less than one second, but took " ++ show td ++ " seconds.") $ td <= 1
