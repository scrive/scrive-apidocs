module SignupTest (signupTests, getAccountCreatedActions) where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Happstack.Server
import Happstack.State (query)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)

import ActionSchedulerState
import Context
import Control.Logic
import DB.Classes
import FlashMessage
import MagicHash (MagicHash)
import Mails.Model
import Login
import LoginTest (assertLoginEventRecordedFor)
import MinutesTime
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import Util.HasSomeUserInfo

signupTests :: DBEnv -> Test
signupTests conn = testGroup "Signup" [
      testCase "can self signup and activate an account" $ testSignupAndActivate conn
    , testCase "can send viral invite which can be used to activate an account" $ testViralInviteAndActivate conn
    , testCase "must accept tos to activate an account" $ testAcceptTOSToActivate conn
    , testCase "must enter first name to activate an account" $ testNeedFirstNameToActivate conn
    , testCase "must enter last name to activate an account" $ testNeedLastNameToActivate conn
    , testCase "must enter passwords to activate an account" $ testNeedPasswordToActivate conn
    , testCase "passwords must match to activate an account" $ testPasswordsMatchToActivate conn
    , testCase "login event recorded when logged in after activation" $ testLoginEventRecordedWhenLoggedInAfterActivation conn
    ]

testSignupAndActivate :: DBEnv -> Assertion
testSignupAndActivate conn = withTestEnvironment conn $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = conn })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated uid token) = actionType action

  -- follow the signup link
  (res2, ctx2) <- followActivationLink ctx1 aid token
  assertActivationPageOK (res2, ctx2)

  -- activate the account using the signup details
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "Rybczak" "password12" "password12" (Just "123")
  assertAccountActivatedFor uid "Andrzej" "Rybczak" (res3, ctx3)
  Just uuser <- dbQuery $ GetUserByID  uid
  assertEqual "Phone number was saved" "123" (userphone $ userinfo uuser)
  emails <- dbQuery GetEmails
  assertEqual "An email was sent" 2 (length emails) -- Two mail - one for user and one to info adress.



testLoginEventRecordedWhenLoggedInAfterActivation :: DBEnv -> Assertion
testLoginEventRecordedWhenLoggedInAfterActivation env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated uid token) = actionType action

  -- activate the account using the signup details
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivatedFor uid "Andrzej" "Rybczak" (res3, ctx3)
  assertLoginEventRecordedFor uid

testViralInviteAndActivate :: DBEnv -> Assertion
testViralInviteAndActivate env = withTestEnvironment env $ do
  Just inviter <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just inviter })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

   -- enter the email to invite
  (res1, ctx1) <- inviteToAccount ctx "emily@scrive.com"
  action <- assertInviteSuccessful (userid inviter) ("emily@scrive.com") (res1, ctx1)
  let aid = actionID action
      (ViralInvitationSent _ _ _ _ token) = actionType action

  -- follow the signup link
  let ctx1withoutuser = ctx1{ctxmaybeuser = Nothing}
  (res2, ctx2) <- followActivationLink ctx1withoutuser aid token
  assertActivationPageOK (res2, ctx2)

  -- activate the account using the signup details
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivated "Andrzej" "Rybczak" (res3, ctx3)

testAcceptTOSToActivate :: DBEnv -> Assertion
testAcceptTOSToActivate env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated _uid token) = actionType action

  -- activate the account without accepting the tos
  (res3, ctx3) <- activateAccount ctx1 aid token False "Andrzej" "Rybczak" "password12" "password12" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedFirstNameToActivate :: DBEnv -> Assertion
testNeedFirstNameToActivate env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated _uid token) = actionType action

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 aid token True "" "Rybczak" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedLastNameToActivate :: DBEnv -> Assertion
testNeedLastNameToActivate env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated _uid token) = actionType action

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testNeedPasswordToActivate :: DBEnv -> Assertion
testNeedPasswordToActivate env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated _uid token) = actionType action

  -- activate the account without entering passwords
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "Rybczak" "" "" Nothing
  assertAccountActivationFailed (res3, ctx3)

testPasswordsMatchToActivate :: DBEnv -> Assertion
testPasswordsMatchToActivate env = withTestEnvironment env $ do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  -- enter the email to signup
  (res1, ctx1) <- signupForAccount ctx "andrzej@skrivapa.se"
  action <- assertSignupSuccessful (res1, ctx1)
  let aid = actionID action
      (AccountCreated _uid token) = actionType action

  -- activate the account using mismatched passwords
  (res3, ctx3) <- activateAccount ctx1 aid token True "Andrzej" "Rybczak" "password12" "password21" Nothing
  assertAccountActivationFailed (res3, ctx3)

signupForAccount :: MonadIO m => Context -> String -> m (Response, Context)
signupForAccount ctx email = do
  req <- mkRequest POST [("email", inText email)]
  runTestKontra req ctx $ signupPagePost >>= sendRedirect

assertSignupSuccessful :: MonadIO m => (Response, Context) -> m Action
assertSignupSuccessful (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool ("Flash message has type indicating success, was "  ++ show (getFlashType $ head $ ctxflashmessages ctx)) $ head (ctxflashmessages ctx) `isFlashOfType` Modal
  actions <- getAccountCreatedActions
  assertEqual "An AccountCreated action was made" 1 (length $ actions)
  return $ head actions

inviteToAccount :: MonadIO m => Context -> String -> m (Response, Context)
inviteToAccount ctx email = do
  req <- mkRequest POST [("invitedemail", inText email)]
  runTestKontra req ctx $ handleViralInvite >>= sendRedirect

assertInviteSuccessful :: MonadIO m => UserID -> String -> (Response, Context) -> m Action
assertInviteSuccessful expectedid expectedemail (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "User is logged in" (Just expectedid) (fmap userid $ ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  -- why is this signing related?!  should be just success no?
  assertBool "Flash message has type indicating its signing related" $ head (ctxflashmessages ctx) `isFlashOfType` SigningRelated
  actions <- getViralInviteActions
  assertEqual "A ViralInvite action was made" 1 (length $ actions)
  let action = head actions
      (ViralInvitationSent email _ inviterid _ _) = actionType action
  assertEqual "Action email is correct" (Email expectedemail) email
  assertEqual "Inviter id is correct" expectedid inviterid
  return action

followActivationLink :: MonadIO m => Context -> ActionID -> MagicHash -> m (Response, Context)
followActivationLink ctx aid token = do
  req <- mkRequest GET []
  runTestKontra req ctx $ handleAccountSetupGet aid token

assertActivationPageOK :: MonadIO m => (Response, Context) -> m ()
assertActivationPageOK (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool "Flash message has type indicating is modal" $ head (ctxflashmessages ctx) `isFlashOfType` Modal

activateAccount :: MonadIO m => Context -> ActionID -> MagicHash -> Bool -> String -> String -> String -> String -> Maybe String -> m (Response, Context)
activateAccount ctx aid token tos fstname sndname password password2 phone = do
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
  runTestKontra req ctx $ handleAccountSetupPost aid token >>= sendRedirect

assertAccountActivatedFor :: MonadIO m => UserID -> String -> String -> (Response, Context) -> m ()
assertAccountActivatedFor uid fstname sndname (res, ctx) = do
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctxmaybeuser ctx)
  assertAccountActivated fstname sndname (res, ctx)

assertAccountActivated :: MonadIO m => String -> String -> (Response, Context) -> m ()
assertAccountActivated fstname sndname (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /upload" (Just "/upload") (T.getHeader "location" (rsHeaders res))
  assertEqual "A flash message" 1 (length $ ctxflashmessages ctx)
  --shouldn't this flash just indicate success and not that it's signing related?!
  assertBool "Flash message has type indicating signing related" $ any (`isFlashOfType` SigningRelated) (ctxflashmessages ctx)
  assertBool "Accepted TOS" $ isJust ((ctxmaybeuser ctx) >>= userhasacceptedtermsofservice)
  assertEqual "First name was set" (Just fstname) (getFirstName <$> ctxmaybeuser ctx)
  assertEqual "Second name was set" (Just sndname) (getLastName <$> ctxmaybeuser ctx)

assertAccountActivationFailed :: MonadIO m => (Response, Context) -> m ()
assertAccountActivationFailed (res, ctx) = do
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /se/sv" (Just "/se/sv") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is not logged in" Nothing (ctxmaybeuser ctx)
  assertEqual "There are two flash messages" 2 (length $ ctxflashmessages ctx)
  -- if they don't accept the tos then the flash is signing related, not sure why
  assertBool "One flash has type indicating a failure or signing related" $ any (\f -> f `isFlashOfType` OperationFailed || f `isFlashOfType` SigningRelated) (ctxflashmessages ctx)
  assertBool "One flash has type indicating a modal (the tos modal)" $ any (`isFlashOfType` Modal) (ctxflashmessages ctx)

getViralInviteActions :: MonadIO m => m [Action]
getViralInviteActions = do
  now <- getMinutesTime
  let expirytime = (7 * 24 * 60 + 1) `minutesAfter` now
  allactions <- query $ GetExpiredActions LeisureAction expirytime
  return $ filter isViralInvite allactions

isViralInvite :: Action -> Bool
isViralInvite action =
  case actionType action of
    (ViralInvitationSent _ _ _ _ _) ->  True
    _ -> False

getAccountCreatedActions :: MonadIO m => m [Action]
getAccountCreatedActions = do
  now <- getMinutesTime
  let expirytime = (24 * 60 + 1) `minutesAfter` now
  allactions <- query $ GetExpiredActions LeisureAction expirytime
  return $ filter isAccountCreated allactions

isAccountCreated :: Action -> Bool
isAccountCreated action =
  case actionType action of
    (AccountCreated _ _ ) ->  True
    _ -> False
