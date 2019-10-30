module SignupTest (signupTests, getAccountCreatedActions) where

import Control.Conditional ((<|), (|>))
import Happstack.Server
import Test.Framework
import Text.JSON (JSValue)
import Text.JSON.FromJSValue (fromJSValueField, withJSValue)

import Context
import DB hiding (query, update)
import MagicHash (MagicHash)
import Mails.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import User.API
import User.Model
import User.UserAccountRequest
import User.UserControl
import Util.HasSomeUserInfo

signupTests :: TestEnvSt -> Test
signupTests env = testGroup
  "Signup"
  [ testThat "can self signup and activate an account" env testSignupAndActivate
  , testThat "login event recorded when logged in after activation"
             env
             testLoginEventRecordedWhenLoggedInAfterActivation
  ]

testSignupAndActivate :: TestEnv ()
testSignupAndActivate = do
  ctx                     <- mkContext defaultLang

  -- enter the email to signup
  ctx1                    <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest {..} <- assertSignupSuccessful ctx1

  -- follow the signup link
  ctx2                    <- followActivationLink ctx1 uarUserID uarToken
  assertActivationPageOK ctx2

  -- activate the account using the signup details
  (res, ctx3) <- activateAccount ctx1
                                 uarUserID
                                 uarToken
                                 True
                                 "Andrzej"
                                 "Rybczak"
                                 "password_8866"
                                 "password_8866"
                                 (Just "123")
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" res ctx3
  Just uuser <- dbQuery $ GetUserByID uarUserID
  assertEqual "Phone number was saved" "123" (userphone $ userinfo uuser)
  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent to the user" 1 (length emails)

testLoginEventRecordedWhenLoggedInAfterActivation :: TestEnv ()
testLoginEventRecordedWhenLoggedInAfterActivation = do
  ctx                     <- mkContext defaultLang

  -- enter the email to signup
  ctx1                    <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest {..} <- assertSignupSuccessful ctx1

  -- activate the account using the signup details
  (res, ctx3)             <- activateAccount ctx1
                                             uarUserID
                                             uarToken
                                             True
                                             "Andrzej"
                                             "Rybczak"
                                             "password_8866"
                                             "password_8866"
                                             Nothing
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" res ctx3

signupForAccount :: Context -> Text -> TestEnv Context
signupForAccount ctx email = do
  req <- mkRequest POST [("email", inText email)]
  snd <$> (runTestKontra req ctx $ apiCallSignup)

assertSignupSuccessful :: Context -> TestEnv UserAccountRequest
assertSignupSuccessful ctx = do
  assertEqual "User is not logged in" Nothing (ctx ^. #ctxMaybeUser)
  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)
  return $ head actions

followActivationLink :: Context -> UserID -> MagicHash -> TestEnv Context
followActivationLink ctx uid token = do
  req <- mkRequest GET []
  fmap snd $ runTestKontra req ctx $ handleAccountSetupGet uid token AccountRequest

assertActivationPageOK :: Context -> TestEnv ()
assertActivationPageOK ctx = do
  assertEqual "User is not logged in" Nothing (ctx ^. #ctxMaybeUser)

activateAccount
  :: Context
  -> UserID
  -> MagicHash
  -> Bool
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> TestEnv (JSValue, Context)
activateAccount ctx uid token tos fstname sndname password password2 phone = do
  let tosValue = if tos then "on" else "off"
  req <-
    mkRequest POST
    $  [ ("tos"      , inText tosValue)
       , ("fstname"  , inText fstname)
       , ("sndname"  , inText sndname)
       , ("password" , inText password)
       , ("password2", inText password2)
       ]
    ++ ([("phone", inText $ fromJust phone)] <| isJust phone |> [])
  (res, ctx') <- runTestKontra req ctx $ handleAccountSetupPost uid token AccountRequest
  return (res, ctx')

assertAccountActivatedFor :: UserID -> Text -> Text -> JSValue -> Context -> TestEnv ()
assertAccountActivatedFor uid fstname sndname res ctx = do
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctx ^. #ctxMaybeUser)
  assertAccountActivated fstname sndname res ctx

assertAccountActivated :: Text -> Text -> JSValue -> Context -> TestEnv ()
assertAccountActivated fstname sndname res ctx = do
  ((Just resultOk) :: Maybe Bool) <- withJSValue res $ fromJSValueField "ok"
  assertEqual "Account activation succeeded" True resultOk
  assertBool "Accepted TOS"
    $ isJust (ctx ^. #ctxMaybeUser >>= userhasacceptedtermsofservice)
  assertEqual "First name was set"  (Just fstname) (getFirstName <$> ctx ^. #ctxMaybeUser)
  assertEqual "Second name was set" (Just sndname) (getLastName <$> ctx ^. #ctxMaybeUser)

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredUserAccountRequestsForTesting expirytime
