module LoginTest (loginTests, assertLoginEventRecordedFor) where

import Control.Applicative
import Data.List
import Happstack.Server
import Test.Framework

import ActionQueue.PasswordReminder
import DB
import Context
import FlashMessage
import Login
import Redirect
import Stats.Model
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Misc

loginTests :: TestEnvSt -> Test
loginTests env = testGroup "Login" [
      testThat "can login with valid user and password" env testSuccessfulLogin
    , testThat "can't login with invalid user" env testCantLoginWithInvalidUser
    , testThat "can't login with invalid password" env testCantLoginWithInvalidPassword
    , testThat "logging in records a user login stat event" env testSuccessfulLoginSavesAStatEvent
    , testThat "you get logged in after you reset a password" env assertResettingPasswordLogsIn
    , testThat "when you're logged in after resetting a password a user login stat event is recorded" env assertResettingPasswordRecordsALoginEvent
    ]

testSuccessfulLogin :: TestEnv ()
testSuccessfulLogin = do
    uid <- createTestUser
    ctx <- mkContext (mkLocaleFromRegion defaultValue)
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location is /upload" $ T.getHeader "location" (rsHeaders res) == Just "/upload"
    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testCantLoginWithInvalidUser :: TestEnv ()
testCantLoginWithInvalidUser = do
    _ <- createTestUser
    ctx <- mkContext (mkLocaleFromRegion defaultValue)
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: TestEnv ()
testCantLoginWithInvalidPassword = do
    _ <- createTestUser
    ctx <- mkContext (mkLocaleFromRegion defaultValue)
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testSuccessfulLoginSavesAStatEvent :: TestEnv ()
testSuccessfulLoginSavesAStatEvent = do
  uid <- createTestUser
  ctx <- mkContext (mkLocaleFromRegion defaultValue)
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (_res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
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

createUserAndResetPassword :: TestEnv (User, Response, Context)
createUserAndResetPassword = do
  pwd <- createPassword "admin"
  Just user <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) Nothing Nothing (mkLocaleFromRegion defaultValue)
  PasswordReminder{..} <- newPasswordReminder $ userid user
  ctx <- mkContext (mkLocaleFromRegion defaultValue)
  req <- mkRequest POST [("password", inText "password123"),
                         ("password2", inText "password123")]
  (res, ctx') <- runTestKontra req ctx $ handlePasswordReminderPost prUserID prToken >>= sendRedirect
  return (user, res, ctx')

assertLoginEventRecordedFor :: UserID -> TestEnv ()
assertLoginEventRecordedFor uid = do
  stats <- dbQuery $ GetUserStatEvents
  let loginstats = filter (\UserStatEvent{usUserID, usQuantity} ->
                              usUserID == uid && usQuantity == UserLogin) stats
  assertEqual "Expected 1 login" 1 (length loginstats)
  assertEqual "Expected amount 1" 1 (usAmount $ head loginstats)

loginFailureChecks :: Response -> Context -> TestEnv ()
loginFailureChecks res ctx = do
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location starts with /se/sv/?logging" $ (isPrefixOf "/se/sv/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
    assertBool "User wasn't logged into context" $ ctxmaybeuser ctx == Nothing
    assertBool "One flash message was added" $ length (ctxflashmessages ctx) == 1
    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx) `isFlashOfType` OperationFailed

createTestUser :: TestEnv UserID
createTestUser = do
    pwd <- createPassword "admin"
    Just User{userid} <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" (Just pwd) Nothing Nothing (mkLocaleFromRegion defaultValue)
    return userid
