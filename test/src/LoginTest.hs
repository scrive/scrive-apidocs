module LoginTest (loginTests, assertLoginEventRecordedFor) where

import Control.Applicative
import Data.List
import DB.Nexus
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.Char8 as BS

import ActionSchedulerState
import AppControl
import DB.Classes
import Context
import FlashMessage
import Redirect
import StateHelper
import Stats.Model
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Misc

loginTests :: Nexus -> Test
loginTests conn = testGroup "Login" [
      testCase "can login with valid user and password" $ testSuccessfulLogin conn
    , testCase "can't login with invalid user" $ testCantLoginWithInvalidUser conn
    , testCase "can't login with invalid password" $ testCantLoginWithInvalidPassword conn
    , testCase "logging in records a user login stat event" $ testSuccessfulLoginSavesAStatEvent conn
    , testCase "you get logged in after you reset a password" $ assertResettingPasswordLogsIn conn
    , testCase "when you're logged in after resetting a password a user login stat event is recorded" $ assertResettingPasswordRecordsALoginEvent conn
    ]

testSuccessfulLogin :: Nexus -> Assertion
testSuccessfulLogin conn = withTestEnvironment conn $ do
    uid <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location is /upload" $ T.getHeader "location" (rsHeaders res) == Just "/upload"
    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testCantLoginWithInvalidUser :: Nexus -> Assertion
testCantLoginWithInvalidUser conn = withTestEnvironment conn $ do
    _ <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: Nexus -> Assertion
testCantLoginWithInvalidPassword conn = withTestEnvironment conn $ do
    _ <- createTestUser
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testSuccessfulLoginSavesAStatEvent :: Nexus -> Assertion
testSuccessfulLoginSavesAStatEvent conn = withTestEnvironment conn $ do
  uid <- createTestUser
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (_res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
  assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
  assertLoginEventRecordedFor uid

assertResettingPasswordLogsIn :: Nexus -> Assertion
assertResettingPasswordLogsIn conn = withTestEnvironment conn $ do
  (user, _res, ctx) <- createUserAndResetPassword conn
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> ctxmaybeuser ctx)

assertResettingPasswordRecordsALoginEvent :: Nexus -> Assertion
assertResettingPasswordRecordsALoginEvent conn = withTestEnvironment conn $ do
  (user, _res, ctx) <- createUserAndResetPassword conn
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> ctxmaybeuser ctx)
  assertLoginEventRecordedFor (userid user)

createUserAndResetPassword :: Nexus -> DB (User, Response, Context)
createUserAndResetPassword conn = do
  pwd <- createPassword $ BS.pack "admin"
  Just user <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") (Just pwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
  Action{ actionID, actionType = PasswordReminder { prToken } } <- newPasswordReminder user
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  req <- mkRequest POST [("password", inText "password123"),
                         ("password2", inText "password123")]
  (res, ctx') <- runTestKontra req ctx $ handlePasswordReminderPost actionID prToken >>= sendRedirect
  return (user, res, ctx')

assertLoginEventRecordedFor :: UserID -> DB ()
assertLoginEventRecordedFor uid = do
  stats <- dbQuery $ GetUserStatEvents
  let loginstats = filter (\UserStatEvent{usUserID, usQuantity} ->
                              usUserID == uid && usQuantity == UserLogin) stats
  assertEqual "Expected 1 login" 1 (length loginstats)
  assertEqual "Expected amount 1" 1 (usAmount $ head loginstats)

loginFailureChecks :: Response -> Context -> DB ()
loginFailureChecks res ctx = do
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location starts with /se/sv/?logging" $ (isPrefixOf "/se/sv/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
    assertBool "User wasn't logged into context" $ ctxmaybeuser ctx == Nothing
    assertBool "One flash message was added" $ length (ctxflashmessages ctx) == 1
    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx) `isFlashOfType` OperationFailed

createTestUser :: DB UserID
createTestUser = do
    pwd <- createPassword $ BS.pack "admin"
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") (Just pwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
    return userid
