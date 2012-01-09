module StatsTest (statsTests) where

import DB.Classes
import User.Model
import Doc.Transitory
import Doc.DocUtils
import Doc.DocStateData
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MinutesTime
import Test.HUnit.Base (Assertion)
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Data.ByteString.UTF8 as BS

import Data.Functor
import Data.Maybe
import Data.Convertible(convert)
import Database.HDBC(SqlValue)
import Database.HDBC.PostgreSQL
import Control.Monad
import Control.Monad.Trans
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import File.FileID
import Stats.Control
import Stats.Model
import StateHelper
import Control.Monad.Reader
import DB.Classes
import qualified AppLogger as Log
import Mails.MailsUtil

statsTests :: Connection -> Test
statsTests conn = testGroup "Stats" 
  [
    testCase "test invite stat" $ testInviteStat conn
  , testCase "test receive stat" $ testReceiveStat conn
  , testCase "test open stat" $ testOpenStat conn   
  , testCase "test link stat" $ testLinkStat conn
  , testCase "test sign stat" $ testSignStat conn
  , testCase "test reject stat" $ testRejectStat conn
  ]

testInviteStat :: Connection -> Assertion
testInviteStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' isSignable
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  _ <- forM (documentsignatorylinks doc) (\sl -> addSignStatInviteEvent doc sl time)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats)
  forM_ stats (\s->assertEqual ("Should be Invite stat but was " ++ show (ssQuantity s)) SignStatInvite (ssQuantity s))

testReceiveStat :: Connection -> Assertion
testReceiveStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' isSignable
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  docs <- forM (documentsignatorylinks doc) (\sl -> dbUpdate $ SetInvitationDeliveryStatus (documentid doc') (signatorylinkid sl) Delivered)
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  _ <- forM (documentsignatorylinks d) (\sl -> addSignStatReceiveEvent d sl time)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats)
  forM_ stats (\s->assertEqual "Wrong stat type" SignStatReceive (ssQuantity s))

testOpenStat :: Connection -> Assertion
testOpenStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^ isPreparation)
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  _ <- forM (documentsignatorylinks doc) (\sl -> dbUpdate $ MarkInvitationRead (documentid doc) (signatorylinkid sl) time)
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  forM (documentsignatorylinks d) (\sl -> Log.debug $ "maybereadinvite: " ++ show (maybereadinvite sl))
  _ <- forM (documentsignatorylinks d) (\sl -> addSignStatOpenEvent d sl)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks d) (length stats)
  forM_ stats (\s->assertEqual "Wrong stat type" SignStatOpen (ssQuantity s))

testLinkStat :: Connection -> Assertion
testLinkStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^ isPreparation)
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  docs <- forM (documentsignatorylinks doc) (\sl -> dbUpdate $ MarkDocumentSeen 
                                                    (documentid doc') 
                                                    (signatorylinkid sl)
                                                    (signatorymagichash sl)
                                                    time
                                                    (IPAddress 0))
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  _ <- forM (documentsignatorylinks d) (\sl -> addSignStatLinkEvent d sl)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats)
  forM_ stats (\s->assertEqual "Wrong stat type" SignStatLink (ssQuantity s))

testSignStat :: Connection -> Assertion
testSignStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^ isPreparation)
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  docs <- forM (documentsignatorylinks doc) (\sl -> dbUpdate $ SignDocument
                                                    (documentid doc') 
                                                    (signatorylinkid sl)
                                                    (signatorymagichash sl)
                                                    time
                                                    (IPAddress 0)
                                                    Nothing)
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  _ <- forM (documentsignatorylinks d) (\sl -> addSignStatSignEvent d sl)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats)
  forM_ stats (\s->assertEqual "Wrong stat type" SignStatSign (ssQuantity s))

testRejectStat :: Connection -> Assertion
testRejectStat conn = withTestEnvironment conn $ do
  stats' <- dbQuery $ GetSignStatEvents
  assertEqual "Stats should be empty." 0 (length stats')
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  doc' <- addRandomDocumentWithAuthorAndCondition author' (isSignable &&^ isPreparation)
  time <- getMinutesTime
  _ <- dbUpdate $ PreparationToPending (documentid doc') time
  Right doc <- dbUpdate $ SetDocumentInviteTime (documentid doc') time (IPAddress 0)
  docs <- forM (documentsignatorylinks doc) (\sl -> dbUpdate $ RejectDocument
                                                    (documentid doc') 
                                                    (signatorylinkid sl)
                                                    time
                                                    (IPAddress 0)
                                                    Nothing)
  Just d <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  _ <- forM (documentsignatorylinks d) (\sl -> addSignStatRejectEvent d sl)
  stats <- dbQuery $ GetSignStatEvents
  assertEqual "Should have saved stat." (length $ documentsignatorylinks doc) (length stats)
  forM_ stats (\s->assertEqual "Wrong stat type" SignStatReject (ssQuantity s))






instance DBMonad DB where
  getConnection = DB $ ask
    
  handleDBError e = do
    Log.error $ show e
    mzero
