module LoginTest (loginTests, assertLoginEventRecordedFor) where

import Control.Applicative
import Happstack.Server
import Test.Framework

import ActionQueue.PasswordReminder
import DB
import Context
import Login
import Stats.Model
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Utils.Default
import Text.JSON.Gen as J
import Text.JSON

loginTests :: TestEnvSt -> Test
loginTests env = testGroup "Login" [
      testThat "can login with valid user and password" env testSuccessfulLogin
    , testThat "can login to pad queue" env testSuccessfulLoginToPadQueue
    , testThat "can't login with invalid user" env testCantLoginWithInvalidUser
    , testThat "can't login with invalid password" env testCantLoginWithInvalidPassword
    , testThat "logging in records a user login stat event" env testSuccessfulLoginSavesAStatEvent
    , testThat "you get logged in after you reset a password" env assertResettingPasswordLogsIn
    , testThat "when you're logged in after resetting a password a user login stat event is recorded" env assertResettingPasswordRecordsALoginEvent
    ]

testSuccessfulLogin :: TestEnv ()
testSuccessfulLogin = do
    uid <- createTestUser
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" True)
    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
    assertBool "User was not logged into context as pad user" $ ctxmaybepaduser ctx' == Nothing
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testSuccessfulLoginToPadQueue :: TestEnv ()
testSuccessfulLoginToPadQueue  = do
    uid <- createTestUser
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("pad", inText "true")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" True)
    assertBool "User was logged into context as pad user" $ (userid <$> ctxmaybepaduser ctx') == Just uid
    assertBool "User was not logged into context" $ ctxmaybeuser ctx' == Nothing
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testCantLoginWithInvalidUser :: TestEnv ()
testCantLoginWithInvalidUser = do
    _ <- createTestUser
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: TestEnv ()
testCantLoginWithInvalidPassword = do
    _ <- createTestUser
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    loginFailureChecks res ctx'

testSuccessfulLoginSavesAStatEvent :: TestEnv ()
testSuccessfulLoginSavesAStatEvent = do
  uid <- createTestUser
  ctx <- mkContext defaultValue
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
  (_res, ctx') <- runTestKontra req ctx $ handleLoginPost
  assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
  assertLoginEventRecordedFor uid

assertResettingPasswordLogsIn :: TestEnv ()
assertResettingPasswordLogsIn = do
  (user, _res, ctx) <- createUserAndResetPassword
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> ctxmaybeuser ctx)

assertResettingPasswordRecordsALoginEvent :: TestEnv ()
assertResettingPasswordRecordsALoginEvent = do
  (user, _res, ctx) <- createUserAndResetPassword
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> ctxmaybeuser ctx)
  assertLoginEventRecordedFor (userid user)

createUserAndResetPassword :: TestEnv (User, JSValue, Context)
createUserAndResetPassword = do
  pwd <- createPassword "admin"
  Just user <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) Nothing defaultValue
  PasswordReminder{..} <- newPasswordReminder $ userid user
  ctx <- mkContext defaultValue
  req <- mkRequest POST [("password", inText "password123")]
  (_, ctx') <- runTestKontra req ctx $ handlePasswordReminderPost prUserID prToken
  req2 <- mkRequest GET []
  (res, ctx'') <- runTestKontra req2 ctx' getUserJSON
  return (user, res, ctx'')

assertLoginEventRecordedFor :: UserID -> TestEnv ()
assertLoginEventRecordedFor uid = do
  stats <- dbQuery $ GetUserStatEvents
  let loginstats = filter (\UserStatEvent{usUserID, usQuantity} ->
                              usUserID == uid && usQuantity == UserLogin) stats
  assertEqual "Expected 1 login" 1 (length loginstats)
  assertEqual "Expected amount 1" 1 (usAmount $ head loginstats)

loginFailureChecks :: JSValue -> Context -> TestEnv ()
loginFailureChecks res ctx = do
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" False)
    assertBool "User wasn't logged into context" $ ctxmaybeuser ctx == Nothing
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx

createTestUser :: TestEnv UserID
createTestUser = do
    pwd <- createPassword "admin"
    Just User{userid} <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) Nothing defaultValue
    return userid
