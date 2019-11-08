module Doc.API.V2.APILogTest (apiV2CallLogTests) where

import Data.Function (on)
import Data.List (sortBy)
import Happstack.Server
import Test.Framework

import DB
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Mock.TestUtils
import TestingUtil
import TestKontra
import User.APILog.API
import User.APILog.Model
import User.Lang (defaultLang)

apiV2CallLogTests :: TestEnvSt -> Test
apiV2CallLogTests env =
  testGroup "APILog"
    $ [ testThat "API Call gets logged"                  env testApiLogIsStored
      , testThat "API log is rotated"                    env testApiLogIsRotated
      , testThat "Getting a single log item works"       env testApiLogGetItemWorks
      , testThat "Getting list of logs works"            env testApiLogGetListWorks
      , testThat "Multiple users can work independently" env testApiLog2Users
      ]

testApiLogIsStored :: TestEnv ()
testApiLogIsStored = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2New' ctx
  let did = getMockDocId newMockDoc

  setRequestURI "/api/v2/documents"
  void $ testRequestHelper ctx GET [] (docApiV2Get did) 200
  logs <- dbQuery $ GetCallLogList (user ^. #id)
  assertEqual "There should be a logged API Call" 1 (length logs)


testApiLogIsRotated :: TestEnv ()
testApiLogIsRotated = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2New' ctx
  let did = getMockDocId newMockDoc

  setRequestURI "/api/v2/documents"
  forM_ [1 .. 101] $ \_ -> testRequestHelper ctx GET [] (docApiV2Get did) 200
  logs <- dbQuery $ GetCallLogList (user ^. #id)
  assertEqual "There should be only 100 logged API Calls" 100 (length logs)

testApiLogGetItemWorks :: TestEnv ()
testApiLogGetItemWorks = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2New' ctx
  let did = getMockDocId newMockDoc

  setRequestURI "/api/v2/documents"
  forM_ [1 .. 5] $ \_ -> testRequestHelper ctx GET [] (docApiV2Get did) 200
  logs <- dbQuery $ GetCallLogList (user ^. #id)
  assertEqual "There should be 5 logged API Calls" 5 (length logs)
  void $ testRequestHelper ctx GET [] (apiLogGetItem . cliID . head $ logs) 200

testApiLogGetListWorks :: TestEnv ()
testApiLogGetListWorks = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2New' ctx
  let did = getMockDocId newMockDoc

  setRequestURI "/api/v2/documents"
  forM_ [1 .. 5] $ \_ -> testRequestHelper ctx GET [] (docApiV2Get did) 200
  void $ testRequestHelper ctx GET [] apiLogGetList 200

testApiLog2Users :: TestEnv ()
testApiLog2Users = do
  userA       <- addNewRandomUser
  ctxA        <- (set #maybeUser (Just userA)) <$> mkContext defaultLang
  newMockDocA <- testDocApiV2New' ctxA
  let didA = getMockDocId newMockDocA

  setRequestURI "/api/v2/documents"
  forM_ [1 .. 5] $ \_ -> testRequestHelper ctxA GET [] (docApiV2Get didA) 200
  logsUserA <- dbQuery $ GetCallLogList (userA ^. #id)
  assertEqual "There should be 5 logged API Calls for user A" 5 (length logsUserA)

  userB       <- addNewRandomUser
  ctxB        <- (set #maybeUser (Just userB)) <$> mkContext defaultLang
  newMockDocB <- testDocApiV2New' ctxB
  let didB = getMockDocId newMockDocB

  forM_ [1 .. 101] $ \_ -> testRequestHelper ctxB GET [] (docApiV2Get didB) 200
  logsUserB <- dbQuery $ GetCallLogList (userB ^. #id)
  assertEqual "There should be only 100 logged API Calls for user B"
              100
              (length logsUserB)

  logsUserA' <- dbQuery $ GetCallLogList (userA ^. #id)
  assertEqual "Logs of user A are not affected by user B activity"
              (sortBy (compare `on` cliID) logsUserA)
              (sortBy (compare `on` cliID) logsUserA')
