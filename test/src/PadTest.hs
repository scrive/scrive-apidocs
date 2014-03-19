module PadTest (padTests) where

import Control.Logic
import DB
import Data.List
import User.Model
import Doc.DocUtils
import Doc.DocStateData
import Doc.DocumentMonad (withDocument)
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil

import Test.Framework
import TestKontra
import Util.Actor
import Utils.Default (defaultValue)

import PadQueue.Model

padTests :: TestEnvSt -> Test
padTests env = testGroup "Pad"
               [ padQueueTests env
               ]


padQueueTests :: TestEnvSt -> Test
padQueueTests env = testGroup "PadQueue" [
    testThat "test that we can add to padqueue and padqueue has propper data" env testAddToPadQueue
  , testThat "test test that we can remove document from padque"    env testRemoveFromPadQueue
  , testThat "test that we replace document in padqueue on insert" env testLastDocumentInPadqueue
  ]


testAddToPadQueue :: TestEnv ()
testAddToPadQueue = do
  user <- addNewRandomUser
  ctx <- mkContext defaultValue
  let aa = authorActor ctx user
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           (any (((==) PadDelivery) . signatorylinkdeliverymethod) . documentsignatorylinks) )
  let Just siglink = find ((==) PadDelivery . signatorylinkdeliverymethod) (documentsignatorylinks doc)
  _ <- withDocument doc $ dbUpdate $ AddToPadQueue (userid user) (signatorylinkid $ siglink) aa
  Just (did,slid) <- dbQuery $ GetPadQueue (userid user)
  assertEqual "We get from pad queue same document we put in" (documentid doc) did
  assertEqual "We get from pad queue same document we put in" (signatorylinkid $ siglink) slid

testRemoveFromPadQueue :: TestEnv ()
testRemoveFromPadQueue = do
  user <- addNewRandomUser
  ctx <- mkContext defaultValue
  let aa = authorActor ctx user
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           (any (((==) PadDelivery) . signatorylinkdeliverymethod) . documentsignatorylinks))
  let Just siglink = find ((==) PadDelivery . signatorylinkdeliverymethod) (documentsignatorylinks doc)
  _ <- withDocument doc $ dbUpdate $ AddToPadQueue (userid user) (signatorylinkid $ siglink) aa
  _ <- dbQuery $ GetPadQueue (userid user)
  _ <- dbUpdate $ ClearPadQueue (userid user) aa
  pq <- dbQuery $ GetPadQueue (userid user)
  assertEqual "After clearing pad queue is empty" Nothing pq


testLastDocumentInPadqueue :: TestEnv ()
testLastDocumentInPadqueue = do
  user <- addNewRandomUser
  ctx <- mkContext defaultValue
  let aa = authorActor ctx user
  doc1 <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           (any (((==) PadDelivery) . signatorylinkdeliverymethod) . documentsignatorylinks))
  let Just siglink1 = find ((==) PadDelivery . signatorylinkdeliverymethod) (documentsignatorylinks doc1)
  _ <- withDocument doc1 $ dbUpdate $ AddToPadQueue (userid user) (signatorylinkid siglink1) aa
  doc2 <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^
                                                           isPending &&^
                                                           (any isSignatory . documentsignatorylinks) &&^
                                                           (any (((==) PadDelivery) . signatorylinkdeliverymethod) . documentsignatorylinks))
  let Just siglink2 = find ((==) PadDelivery . signatorylinkdeliverymethod) (documentsignatorylinks doc2)
  _ <- withDocument doc2 $ dbUpdate $ AddToPadQueue (userid user) (signatorylinkid siglink2) aa
  Just (did,slid) <- dbQuery $ GetPadQueue (userid user)
  assertEqual "We get last document from padqueue" (documentid doc2) did
  assertEqual "We get last document from padqueue with proper signatory link" (signatorylinkid siglink2) slid
