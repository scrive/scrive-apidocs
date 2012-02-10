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

import Data.Maybe

statsTests :: DBEnv -> Test
statsTests env = testGroup "Stats"
  [
    testCase "test invite stat"                  $ testInviteStat       env
  , testCase "test receive stat"                 $ testReceiveStat      env
  , testCase "test open stat"                    $ testOpenStat         env
  , testCase "test link stat"                    $ testLinkStat         env
  , testCase "test sign stat"                    $ testSignStat         env
  , testCase "test reject stat"                  $ testRejectStat       env
  , testCase "test delete stat"                  $ testDeleteStat       env
  , testCase "test purge stat"                   $ testPurgeStat        env
  , testCase "test time efficiency of userstats" $ testGetUsersAndStats env
  ]

testInviteStat :: DBEnv -> Assertion
testInviteStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatInviteEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual ("Should be Invite stat but was " ++ show (ssQuantity s)) SignStatInvite (ssQuantity s))

testReceiveStat :: DBEnv -> Assertion
testReceiveStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ SetInvitationDeliveryStatus (documentid doc') (signatorylinkid sl) Delivered)
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatReceiveEvent doc sl time)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatReceive (ssQuantity s))

testOpenStat :: DBEnv -> Assertion
testOpenStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ MarkInvitationRead (documentid doc') (signatorylinkid sl) time)
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatOpenEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatOpen (ssQuantity s))

testLinkStat :: DBEnv -> Assertion
testLinkStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) time (IPAddress 0))
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatLinkEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatLink (ssQuantity s))

testSignStat :: DBEnv -> Assertion
testSignStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  _ <- forM (documentsignatorylinks doc') (\sl -> dbUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid sl) (signatorymagichash sl) time (IPAddress 0))
  _ <- forM (filter isSignatory $ documentsignatorylinks doc') (\sl -> dbUpdate $ SignDocument (documentid doc') (signatorylinkid sl) (signatorymagichash sl) time (IPAddress 0) Nothing)
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatSignEvent doc sl)
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ filter isSignatory $ documentsignatorylinks doc) (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatSign (ssQuantity s))

testRejectStat :: DBEnv -> Assertion
testRejectStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl' = getAuthorSigLink doc
  _ <- dbUpdate $ RejectDocument (documentid doc)
         (signatorylinkid asl')
         time
         (IPAddress 0)
         Nothing
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  let Just asl = getAuthorSigLink d
  _ <- addSignStatRejectEvent d asl
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatReject (ssQuantity s))

testDeleteStat :: DBEnv -> Assertion
testDeleteStat env = withTestEnvironment env $ do
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
  _ <- dbUpdate $ ArchiveDocument author' (documentid doc')

  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl = getAuthorSigLink doc
  _ <- addSignStatDeleteEvent doc asl time
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatDelete (ssQuantity s))

testPurgeStat :: DBEnv -> Assertion
testPurgeStat env = withTestEnvironment env $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) Nothing
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^
                                                           isClosed &&^
                                                           (any isSignatory . documentsignatorylinks))
  time <- getMinutesTime
  _ <- dbUpdate $ ArchiveDocument author' (documentid doc')
  _ <- dbUpdate $ ReallyDeleteDocument author' (documentid doc')
  Just doc <- dbQuery $ GetDocumentByDocumentID (documentid doc')
  let Just asl = getAuthorSigLink doc
  _ <- addSignStatPurgeEvent doc asl time
  stats'' <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." 1 (length stats'')
  forM_ stats'' (\s->assertEqual "Wrong stat type" SignStatPurge (ssQuantity s))

-- tests for old stats

testGetUsersAndStats :: DBEnv -> Assertion
testGetUsersAndStats env = withTestEnvironment env $ do
  time <- getMinutesTime
  us <- catMaybes <$> (forM (range (0, 100::Int)) $ \i->
                        dbUpdate $ AddUser (BS.fromString "F", BS.fromString "L") (BS.fromString $ "e" ++ show i ++ "@yoyo.com")
                        Nothing False Nothing Nothing (mkLocale REGION_SE LANG_SE))
  _ <- forM [(u, i) | u <- us, i <- range (0, 10::Int)] $ \(u,_) ->
    dbUpdate $ NewDocument u Nothing (BS.fromString "doc!") (Signable Contract) time

  Log.debug $ "Set up test, now running query."
  t0 <- getMinutesTime
  stats <- dbQuery $ GetUsersAndStats
  Log.debug $ "Number of stats returned: " ++ show (length stats)
  t1 <- getMinutesTime

  let td = toSeconds t1 - toSeconds t0
  assertBool ("Should take less than one second, but took " ++ show td ++ " seconds.") $ td <= 1
