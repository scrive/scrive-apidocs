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
import Login
import LoginTest (assertLoginEventRecordedFor)
import MinutesTime
import Utils.Default
import Redirect
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Util.HasSomeUserInfo

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
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- follow the signup link
  (res2, ctx2) <- followActivationLink ctx1 uarUserID uarToken
  assertActivationPageOK (res2, ctx2)

  -- activate the account using the signup details
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password12" (Just "123")
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" (res3, ctx3)
  Just uuser <- dbQuery $ GetUserByID  uarUserID
  assertEqual "Phone number was saved" "123" (userphone $ userinfo uuser)
  emails <- dbQuery GetEmails
  assertEqual "An email was sent" 2 (length emails) -- Two mail - one for user and one to info adress.

testLoginEventRecordedWhenLoggedInAfterActivation :: TestEnv ()
testLoginEventRecordedWhenLoggedInAfterActivation = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account using the signup details
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivatedFor uarUserID "Andrzej" "Rybczak" (res3, ctx3)
  assertLoginEventRecordedFor uarUserID

testAcceptTOSToActivate :: TestEnv ()
testAcceptTOSToActivate = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account without accepting the tos
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken False "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedFirstNameToActivate :: TestEnv ()
testNeedFirstNameToActivate = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "" "Rybczak" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedLastNameToActivate :: TestEnv ()
testNeedLastNameToActivate = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedPasswordToActivate :: TestEnv ()
testNeedPasswordToActivate = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testPasswordsMatchToActivate :: TestEnv ()
testPasswordsMatchToActivate = do
  ctx <- mkContext (mkLocaleFromRegion defaultValue)

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  UserAccountRequest{..} <- assertSignupSuccessful (res1, ctx1)

  -- activate the account using mismatched passwords
  (res3, ctx3) <- activateAccount ctx1 uarUserID uarToken True "Andrzej" "Rybczak" "password12" "password21" Nothing
  assertAccountActivationFailed (res3, ctx3)

signupForAccount :: Context -> String -> TestEnv (Response, Context)
signupForAccount ctx email = do
  req <- mkRequest POST [("email", inText email)]
  runTestKontra req ctx $ signupPagePost >>= sendRedirect

assertSignupSuccessful :: (Response, Context) -> TestEnv UserAccountRequest
assertSignupSuccessful (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool ("Flash message has type indicating success, was "  ++ show (getFlashType $ head $ ctxflashmessages ctx)) $ head (ctxflashmessages ctx) `isFlashOfType` Modal
  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)
  return $ head actions

followActivationLink :: Context -> UserID -> MagicHash -> TestEnv (Response, Context)
followActivationLink ctx uid token = do
  req <- mkRequest GET []
  runTestKontra req ctx $ handleAccountSetupGet uid token

assertActivationPageOK :: (Response, Context) -> TestEnv ()
assertActivationPageOK (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool "Flash message has type indicating is modal" $ head (ctxflashmessages ctx) `isFlashOfType` Modal

activateAccount :: Context -> UserID -> MagicHash -> Bool -> String -> String -> String -> String -> Maybe String -> TestEnv (Response, Context)
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
  runTestKontra req ctx $ handleAccountSetupPost uid token >>= sendRedirect

assertAccountActivatedFor :: UserID -> String -> String -> (Response, Context) -> TestEnv ()
assertAccountActivatedFor uid fstname sndname (res, ctx) = do
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctxmaybeuser ctx)
  assertAccountActivated fstname sndname (res, ctx)

assertAccountActivated :: String -> String -> (Response, Context) -> TestEnv ()
assertAccountActivated fstname sndname (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /d" (Just "/d") (T.getHeader "location" (rsHeaders res))
  assertEqual "A flash message" 1 (length $ ctxflashmessages ctx)
  --shouldn't this flash just indicate success and not that it's signing related?!
  assertBool "Flash message has type indicating signing related" $ any (`isFlashOfType` SigningRelated) (ctxflashmessages ctx)
  assertBool "Accepted TOS" $ isJust ((ctxmaybeuser ctx) >>= userhasacceptedtermsofservice)
  assertEqual "First name was set" (Just fstname) (getFirstName <$> ctxmaybeuser ctx)
  assertEqual "Second name was set" (Just sndname) (getLastName <$> ctxmaybeuser ctx)

assertAccountActivationFailed :: (Response, Context) -> TestEnv ()
assertAccountActivationFailed (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "There are two flash messages" 2 (length $ ctxflashmessages ctx)
  -- if they don't accept the tos then the flash is signing related, not sure why
  assertBool "One flash has type indicating a failure or signing related" $ any (\f -> f `isFlashOfType` OperationFailed || f `isFlashOfType` SigningRelated) (ctxflashmessages ctx)
  assertBool "One flash has type indicating a modal (the tos modal)" $ any (`isFlashOfType` Modal) (ctxflashmessages ctx)

getAccountCreatedActions :: TestEnv [UserAccountRequest]
getAccountCreatedActions = do
  expirytime <- (30 `daysAfter`) <$> getMinutesTime
  dbQuery $ GetExpiredActions userAccountRequest expirytime
