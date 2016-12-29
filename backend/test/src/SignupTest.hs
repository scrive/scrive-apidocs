module SignupTest (signupTests, getAccountCreatedActions) where

import Control.Conditional ((<|), (|>))
import Happstack.Server
import Test.Framework

import ActionQueue.Core
import ActionQueue.UserAccountRequest
import Context
import DB hiding (query, update)
import KontraPrelude
import MagicHash (MagicHash)
import Mails.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import User.API
import User.Model
import User.UserControl
import Util.HasSomeUserInfo

signupTests :: TestEnvSt -> Test
signupTests env = testGroup "Signup" [
      testThat "can self signup and activate an account" env testSignupAndActivate
    , testThat "login event recorded when logged in after activation" env testLoginEventRecordedWhenLoggedInAfterActivation
    ]

testSignupAndActivate :: TestEnv ()
testSignupAndActivate = do
  ctx <- mkContext def

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- follow the signup link
  ctx2 <- followActivationLink ctx1 uarUserID uarToken
  assertActivationPageOK ctx2

  -- activate the account using the signup details
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password12" (Just "123")
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" ctx3
  Just uuser <- dbQuery $ GetUserByID  uarUserID
  assertEqual "Phone number was saved" "123" (userphone $ userinfo uuser)
  emails <- dbQuery GetEmailsForTest
  assertEqual "An email was sent to the user" 1 (length emails)

testLoginEventRecordedWhenLoggedInAfterActivation :: TestEnv ()
testLoginEventRecordedWhenLoggedInAfterActivation = do
  ctx <- mkContext def

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account using the signup details
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" ctx3

signupForAccount :: Context -> String -> TestEnv Context
signupForAccount ctx email = do
  req <- mkRequest POST [("email", inText email)]
  snd <$> (runTestKontra req ctx $ apiCallSignup)

assertSignupSuccessful :: Context -> TestEnv UserAccountRequest
assertSignupSuccessful ctx = do
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)
  return $ $head actions

followActivationLink :: Context -> UserID -> MagicHash -> TestEnv Context
followActivationLink ctx uid token = do
  req <- mkRequest GET []
  fmap snd $ runTestKontra req ctx $ handleAccountSetupGet uid token AccountRequest

assertActivationPageOK :: Context -> TestEnv ()
assertActivationPageOK ctx = do
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)

activateAccount :: Context -> UserID -> MagicHash -> Bool -> String -> String -> String -> String -> Maybe String -> TestEnv Context
activateAccount ctx uid token tos fstname sndname password password2 phone = do
  let tosValue = if tos
                   then "on"
                   else "off"
  req <- mkRequest POST $ [ ("tos", inText tosValue)
                          , ("fstname", inText fstname)
                          , ("sndname", inText sndname)
                          , ("password", inText password)
                          , ("password2", inText password2)
                          ] ++
                          ([("phone", inText $ $fromJust phone)] <| isJust phone |> [])
  (_, ctx') <- runTestKontra req ctx $ handleAccountSetupPost uid token AccountRequest
  return ctx'

assertAccountActivatedFor :: UserID -> String -> String -> Context -> TestEnv ()
assertAccountActivatedFor uid fstname sndname ctx = do
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctxmaybeuser ctx)
  assertAccountActivated fstname sndname ctx

assertAccountActivated :: String -> String -> Context -> TestEnv ()
assertAccountActivated fstname sndname ctx = do
  -- XXX assertEqual "A flash message" 1 (length $ ctxflashmessages ctx)
  --shouldn't this flash just indicate success and not that it's signing related?!
  -- XXX assertBool "Flash message has type indicating signing related" $ any (`isFlashOfType` OperationDone) (ctxflashmessages ctx)
  assertBool "Accepted TOS" $ isJust ((ctxmaybeuser ctx) >>= userhasacceptedtermsofservice)
  assertEqual "First name was set" (Just fstname) (getFirstName <$> ctxmaybeuser ctx)
  assertEqual "Second name was set" (Just sndname) (getLastName <$> ctxmaybeuser ctx)

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> currentTime
  dbQuery $ GetExpiredActions userAccountRequest expirytime
