module PadTest (padTests) where

import Control.Logic
import DB
import User.Model
import Doc.DocUtils
import Doc.DocStateData
import IPAddress
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import MinutesTime

import Test.Framework
import TestKontra
import Util.Actor


import Util.HasSomeUserInfo
import PadQueue.Model

padTests :: TestEnvSt -> Test
padTests env = testGroup "Pad" [  padQueueTests env
                , padSigningTests env
               ]
                

padQueueTests :: TestEnvSt -> Test
padQueueTests env = testGroup "PadQueue" [
    testThat "test that we can add to padqueue and padqueue has propper data" env testAddToPadQueue
  , testThat "test test that we can remove document from padque"    env testRemoveFromPadQueue
  , testThat "test that we replace document in padqueue on insert" env testLastDocumentInPadqueue
  ]


padSigningTests :: TestEnvSt -> Test
padSigningTests env = testGroup "Pad" [
    testThat "Test that for pad signing identification sendMailDurringSigning returns false" env testSendDocumentsDurringSigningReturnsFalseForPad
  ]
 
testSendDocumentsDurringSigningReturnsFalseForPad :: TestEnv ()
testSendDocumentsDurringSigningReturnsFalseForPad = do
  author <- addNewRandomUser
  doc <-  addRandomDocumentWithAuthorAndCondition author (((==) PadDelivery) . documentdeliverymethod)
  assertBool "sendMailsDuringSigning returns false for padsigning documents" (not $ sendMailsDuringSigning doc)


testAddToPadQueue :: TestEnv ()
testAddToPadQueue = do
  user <- addNewRandomUser
  time <- getMinutesTime
  let aa = authorActor time noIP (userid user) (getEmail user)
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           ((==) PadDelivery) . documentdeliverymethod )
  _ <- dbUpdate $ AddToPadQueue (userid user) (documentid doc) (signatorylinkid $ head $ documentsignatorylinks doc) aa
  Just (did,slid) <- dbQuery $ GetPadQueue (userid user)
  assertBool "We get from pad queue same we put in"  (did == documentid doc && slid == (signatorylinkid $ head $ documentsignatorylinks doc))

testRemoveFromPadQueue :: TestEnv ()
testRemoveFromPadQueue = do
  user <- addNewRandomUser
  time <- getMinutesTime
  let aa = authorActor time noIP (userid user) (getEmail user)
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           ((==) PadDelivery) . documentdeliverymethod)
  _ <- dbUpdate $ AddToPadQueue (userid user) (documentid doc) (signatorylinkid $ head $ documentsignatorylinks doc) aa
  _ <- dbQuery $ GetPadQueue (userid user)
  _ <- dbUpdate $ ClearPadQueue (userid user) aa
  pq <- dbQuery $ GetPadQueue (userid user)
  assertBool "After clearing pad queue is empty"  (pq == Nothing)


testLastDocumentInPadqueue :: TestEnv ()
testLastDocumentInPadqueue = do
  user <- addNewRandomUser
  time <- getMinutesTime
  let aa = authorActor time noIP (userid user) (getEmail user)
  doc1 <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           ((==) PadDelivery) . documentdeliverymethod)
  _ <- dbUpdate $ AddToPadQueue (userid user) (documentid doc1) (signatorylinkid $ head $ documentsignatorylinks doc1) aa
  doc2 <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           ((==) PadDelivery) . documentdeliverymethod)
  _ <- dbUpdate $ AddToPadQueue (userid user) (documentid doc2) (signatorylinkid $ head $ documentsignatorylinks doc2) aa
  Just (did,slid) <- dbQuery $ GetPadQueue (userid user)
  assertBool "We get last document from padqueue"  (did == documentid doc2 && slid == (signatorylinkid $ head $ documentsignatorylinks doc2))



  