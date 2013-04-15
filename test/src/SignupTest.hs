module SignupTest (signupTests, getAccountCreatedActions) where

import Control.Applicative
import Data.Maybe
import Happstack.Server
import Test.Framework

import ActionQueue.Core
import ActionQueue.UserAccountRequest
import Context
import Control.Logic
import DB hiding (query, update)
import FlashMessage
import MagicHash (MagicHash)
import Mails.Model
import LoginTest (assertLoginEventRecordedFor)
import MinutesTime
import Utils.Default
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Util.HasSomeUserInfo
import User.API

signupTests :: TestEnvSt -> Test
signupTests env = testGroup "Signup" [
      testThat "can self signup and activate an account" env testSignupAndActivate
    , testThat "must accept tos to activate an account" env testAcceptTOSToActivate
    , testThat "must enter first name to activate an account" env testNeedFirstNameToActivate
    , testThat "must enter last name to activate an account" env testNeedLastNameToActivate
    , testThat "must enter passwords to activate an account" env testNeedPasswordToActivate
    , testThat "passwords must match to activate an account" env testPasswordsMatchToActivate
    , testThat "login event recorded when logged in after activation" env testLoginEventRecordedWhenLoggedInAfterActivation
    ]

testSignupAndActivate :: TestEnv ()
testSignupAndActivate = do
  ctx <- mkContext defaultValue

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
  emails <- dbQuery GetEmails
  assertEqual "An email was sent" 2 (length emails) -- Two mail - one for user and one to info adress.

testLoginEventRecordedWhenLoggedInAfterActivation :: TestEnv ()
testLoginEventRecordedWhenLoggedInAfterActivation = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account using the signup details
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" ctx3
  assertLoginEventRecordedFor uarUserID

testAcceptTOSToActivate :: TestEnv ()
testAcceptTOSToActivate = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account without accepting the tos
  ctx3 <- activateAccount ctx1 uarUserID uarToken False "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivationFailed ctx3

testNeedFirstNameToActivate :: TestEnv ()
testNeedFirstNameToActivate = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account without entering passwords
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "" "Rybczak" "" "" Nothing
  assertAccountActivationFailed ctx3

testNeedLastNameToActivate :: TestEnv ()
testNeedLastNameToActivate = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account without entering passwords
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "" "" "" Nothing
  assertAccountActivationFailed ctx3

testNeedPasswordToActivate :: TestEnv ()
testNeedPasswordToActivate = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account without entering passwords
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "" "" Nothing
  assertAccountActivationFailed ctx3

testPasswordsMatchToActivate :: TestEnv ()
testPasswordsMatchToActivate = do
  ctx <- mkContext defaultValue

  -- enter the email to signup
  ctx1 <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful ctx1

  -- activate the account using mismatched passwords
  ctx3 <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password21" Nothing
  assertAccountActivationFailed ctx3

signupForAccount :: Context -> String -> TestEnv Context
signupForAccount ctx email = do
  req <- mkRequest POST [("email", inText email)]
  snd <$> (runTestKontra req ctx $ apiCallSignup)

assertSignupSuccessful :: Context -> TestEnv UserAccountRequest
assertSignupSuccessful ctx = do
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)
  return $ head actions

followActivationLink :: Context -> UserID -> MagicHash -> TestEnv Context
followActivationLink ctx uid token = do
  req <- mkRequest GET []
  fmap snd $ runTestKontra req ctx $ handleAccountSetupGet uid token

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
                          ([("callme", inText "YES"), ("phone", inText $ fromJust phone)] <| isJust phone |> [])
  (_, ctx') <- runTestKontra req ctx $ handleAccountSetupPost uid token
  return ctx'

assertAccountActivatedFor :: UserID -> String -> String -> Context -> TestEnv ()
assertAccountActivatedFor uid fstname sndname ctx = do
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctxmaybeuser ctx)
  assertAccountActivated fstname sndname ctx

assertAccountActivated :: String -> String -> Context -> TestEnv ()
assertAccountActivated fstname sndname ctx = do
  assertEqual "A flash message" 1 (length $ ctxflashmessages ctx)
  --shouldn't this flash just indicate success and not that it's signing related?!
  assertBool "Flash message has type indicating signing related" $ any (`isFlashOfType` SigningRelated) (ctxflashmessages ctx)
  assertBool "Accepted TOS" $ isJust ((ctxmaybeuser ctx) >>= userhasacceptedtermsofservice)
  assertEqual "First name was set" (Just fstname) (getFirstName <$> ctxmaybeuser ctx)
  assertEqual "Second name was set" (Just sndname) (getLastName <$> ctxmaybeuser ctx)

assertAccountActivationFailed :: Context -> TestEnv ()
assertAccountActivationFailed ctx = do
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  -- if they don't accept the tos then the flash is signing related, not sure why

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> getMinutesTime
  dbQuery $ GetExpiredActions userAccountRequest expirytime
