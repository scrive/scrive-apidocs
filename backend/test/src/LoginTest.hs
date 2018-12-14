module LoginTest (loginTests) where

import Data.Aeson
import Data.Text
import Happstack.Server
import Log
import Test.Framework
import Text.JSON hiding (decode)
import Text.JSON.Gen as J
import qualified Data.HashMap.Strict as H

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Context
import DB
import InternalResponse
import KontraLink
import Login
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model
import User.PasswordReminder
import User.UserControl
import UserGroup.Model
import UserGroup.Types

loginTests :: TestEnvSt -> Test
loginTests env = testGroup "Login" [
      testThat "can login with valid user and password" env testSuccessfulLogin
    , testThat "can login to pad queue" env testSuccessfulLoginToPadQueue
    , testThat "can't login with invalid user" env testCantLoginWithInvalidUser
    , testThat "can't login with invalid password" env testCantLoginWithInvalidPassword
    , testThat "logging in records a user login stat event" env testSuccessfulLoginSavesAStatEvent
    , testThat "you get logged in after you reset a password" env assertResettingPasswordLogsIn
    , testThat "when you're logged in after resetting a password a user login stat event is recorded" env assertResettingPasswordRecordsALoginEvent
    , testThat "can use login link" env testCanLoginWithRedirect
    , testThat "can't login after many failed attempts" env testCantLoginAfterFailedAttempts
    ]

testSuccessfulLogin :: TestEnv ()
testSuccessfulLogin = do
    uid <- createTestUser
    ctx <- mkContext defaultLang
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" True)
    assertBool "User was logged into context" $ (userid <$> get ctxmaybeuser ctx') == Just uid
    assertBool "User was not logged into context as pad user" $ get ctxmaybepaduser ctx' == Nothing

testSuccessfulLoginToPadQueue :: TestEnv ()
testSuccessfulLoginToPadQueue  = do
    uid <- createTestUser
    ctx <- mkContext defaultLang
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("pad", inText "true")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" True)
    assertBool "User was logged into context as pad user" $ (userid <$> get ctxmaybepaduser ctx') == Just uid
    assertBool "User was not logged into context" $ get ctxmaybeuser ctx' == Nothing

testCantLoginWithInvalidUser :: TestEnv ()
testCantLoginWithInvalidUser = do
    void $ createTestUser
    ctx <- mkContext defaultLang
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: TestEnv ()
testCantLoginWithInvalidPassword = do
    void $ createTestUser
    ctx <- mkContext defaultLang
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid"), ("loginType", inText "RegularLogin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost
    loginFailureChecks res ctx'

testSuccessfulLoginSavesAStatEvent :: TestEnv ()
testSuccessfulLoginSavesAStatEvent = do
  uid <- createTestUser
  ctx <- mkContext defaultLang
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
  (_res, ctx') <- runTestKontra req ctx $ handleLoginPost
  assertBool "User was logged into context" $ (userid <$> get ctxmaybeuser ctx') == Just uid

testCanLoginWithRedirect :: TestEnv ()
testCanLoginWithRedirect = do
  -- create a user
  password <- rand 10 $ arbString 1 64
  logInfo_ $ "Generated password: " <> pack password
  randomUser <- addNewRandomUserWithPassword password
  -- get access tokens using an email and password
  ctx <- mkContext defaultLang
  req1 <- mkRequest GET
    [ ("email", inText $ unEmail $ useremail $ userinfo randomUser)
    , ("password", inText password)
    ]
  (res1, _) <- runTestKontra req1 ctx $ apiCallGetUserPersonalToken
  -- use access tokens to log in and get cookie-like session id
  req2 <- mkRequest GET [ ("personal_token", inTextBS $ rsBody res1) ]
  (res2, _) <- runTestKontra req2 ctx $ apiCallLoginUserAndGetSession
  let Just (Object respObject2) = decode (rsBody res2) :: Maybe Value
      Just (String sessionid) = H.lookup "session_id" respObject2
  -- get cookie and redirect
  let redirecturl1 = "/arbitrary/url/path"
  req3 <- mkRequest GET
    [ ("session_id", inText . unpack $ sessionid)
    , ("url"       , inText redirecturl1)
    ]
  (res3, ctx3) <- runTestKontra req3 ctx $ handleLoginWithRedirectGet
  assertBool "Session was set" $ get ctxsessionid ctx /= get ctxsessionid ctx3
  assertBool "Redirect was set to provided url" (isRedirect (LinkExternal redirecturl1) res3)

    -- Test that call with fail if "url" for redirection is not provided
  req4 <- mkRequest GET [("session_id", inText . unpack $ sessionid)]
  assertRaisesInternalError $ do
    void $ runTestKontra req4 ctx handleLoginWithRedirectGet
    return ()

  -- Test that session_id is valid for more than one redirect
  let redirecturl2 = "/otherarbitrary/url/path"
  req5 <- mkRequest GET
    [ ("session_id", inText . unpack $ sessionid)
    , ("url"       , inText redirecturl2)
    ]
  (res5, ctx5) <- runTestKontra req5 ctx $ handleLoginWithRedirectGet
  assertBool "Session was set again" $ get ctxsessionid ctx /= get ctxsessionid ctx5
  assertBool "Redirect was set to other url" (isRedirect (LinkExternal redirecturl2) res5)

  -- Test that usage of invalid session_id will work
  req6 <- mkRequest GET
    [ ("session_id", inText "1-100000")
    , ("url"       , inText redirecturl2)
    ]
  (res6, ctx6) <- runTestKontra req6 ctx $ handleLoginWithRedirectGet
  assertBool "ctxsessionid will not be changed if session_id is invalid" $ get ctxsessionid ctx == get ctxsessionid ctx6
  assertBool "Redirect was still set to other url" (isRedirect (LinkExternal redirecturl2) res6)

testCantLoginAfterFailedAttempts :: TestEnv ()
testCantLoginAfterFailedAttempts = do
  void $ createTestUser
  ctx <- mkContext defaultLang
  -- we fail to login 6 times
  req1 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid"), ("loginType", inText "RegularLogin")]
  forM_ [1..6] $ \_ -> do
    (res1, ctx1) <- runTestKontra req1 ctx $ handleLoginPost
    loginFailureChecks res1 ctx1

  -- now even correct password does not work
  req2 <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin"), ("loginType", inText "RegularLogin")]
  (res2, ctx2) <- runTestKontra req2 ctx $ handleLoginPost
  loginFailureChecks res2 ctx2

assertResettingPasswordLogsIn :: TestEnv ()
assertResettingPasswordLogsIn = do
  (user, ctx) <- createUserAndResetPassword
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> get ctxmaybeuser ctx)

assertResettingPasswordRecordsALoginEvent :: TestEnv ()
assertResettingPasswordRecordsALoginEvent = do
  (user, ctx) <- createUserAndResetPassword
  assertEqual "User was logged into context" (Just $ userid user) (userid <$> get ctxmaybeuser ctx)

createUserAndResetPassword :: TestEnv (User, Context)
createUserAndResetPassword = do
  bd <- dbQuery $ GetMainBrandedDomain
  pwd <- createPassword "admin"
  ug <- dbUpdate $ UserGroupCreate def
  Just user <- dbUpdate $ AddUser
    ("", "")
    "andrzej@skrivapa.se"
    (Just pwd)
    (get ugID ug, True)
    defaultLang
    (get bdid bd)
    AccountRequest
  PasswordReminder{..} <- newPasswordReminder $ userid user
  ctx <- mkContext defaultLang
  req <- mkRequest POST [("password", inText "password123")]
  (_, ctx') <- runTestKontra req ctx $ handlePasswordReminderPost prUserID prToken
  req2 <- mkRequest GET []
  (_res, ctx'') <- runTestKontra req2 ctx' apiCallGetUserProfile
  return (user, ctx'')


loginFailureChecks :: JSValue -> Context -> TestEnv ()
loginFailureChecks res ctx = do
    assertBool "Response is propper JSON" $ res == (runJSONGen $ value "logged" False)
    assertBool "User wasn't logged into context" $ get ctxmaybeuser ctx == Nothing

createTestUser :: TestEnv UserID
createTestUser = do
    bd <- dbQuery $ GetMainBrandedDomain
    pwd <- createPassword "admin"
    ug <- dbUpdate $ UserGroupCreate def
    Just User{userid} <- dbUpdate $ AddUser
      ("", "")
      "andrzej@skrivapa.se"
      (Just pwd)
      (get ugID ug, True)
      defaultLang
      (get bdid bd)
      AccountRequest
    return userid
