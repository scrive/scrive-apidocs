module AccountInfoTest (accountInfoTests) where

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
import DB.Classes
import FlashMessage
import MagicHash (unsafeMagicHash)
import Mails.Model
import MinutesTime
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Locale
import User.Model
import User.UserControl
import User.Utils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

accountInfoTests :: DBEnv -> Test
accountInfoTests env = testGroup "AccountInfo" [
      testCase "lets private account upgrade to company account" $ testPrivateToCompanyUpgrade env
    , testCase "lets users change their email addresses" $ testChangeEmailAddress env
    , testCase  "requesting email change fails is the entered emails are mismatched" $ testAddressesMustMatchToRequestEmailChange env
    , testCase "need two addresses to request email changes" $ testNeedTwoAddressesToRequestEmailChange env
    , testCase "need unique email to request email change" $ testNeedEmailToBeUniqueToRequestChange env
    , testCase "need the correct action id to complete email change" $ testEmailChangeFailsIfActionIDIsWrong env
    , testCase "need the correct hash to complete email change" $ testEmailChangeFailsIfMagicHashIsWrong env
    , testCase "need the correct user to complete email change" $ testEmailChangeIfForAnotherUser env
    , testCase "need the email to still be unique to complete email change" $ testEmailChangeFailsIfEmailInUse env
    , testCase "need the password to the correct to complete the email change" $ testEmailChangeFailsIfPasswordWrong env
    , testCase "need the password to be entered to complete the email change" $ testEmailChangeFailsIfNoPassword env
    ]

testPrivateToCompanyUpgrade :: DBEnv -> Assertion
testPrivateToCompanyUpgrade env = withTestEnvironment env $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                "Andrzejj"
                                "Rybczakk"
                                "Tester"
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser env user upgradeinfo

  assertCompanyUpgradeSuccessful (userid user) upgradeinfo (res1, ctx1)

data UpgradeInfo = UpgradeInfo String String String String String

upgradeCompanyForUser :: (MonadIO m, Functor m) =>
                               DBEnv
                            -> User
                            -> UpgradeInfo
                            -> m (Response, Context)
upgradeCompanyForUser env user (UpgradeInfo cname fstname sndname position phone) = do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("createcompany", inText "true")
                        , ("companyname", inText cname)
                        , ("fstname", inText fstname)
                        , ("sndname", inText sndname)
                        , ("companyposition", inText position)
                        , ("phone", inText phone)
                        ]
  runTestKontra req ctx $ handleUserPost >>= sendRedirect

assertCompanyUpgradeSuccessful :: UserID -> UpgradeInfo -> (Response, Context) -> DB ()
assertCompanyUpgradeSuccessful uid (UpgradeInfo cname fstname sndname position phone) (res, ctx) = do
  Just user <- dbQuery $ GetUserByID uid
  mcompany <- getCompanyForUser' user

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "User is logged in" (Just $ userid user) (fmap userid $ ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx) `isFlashOfType` OperationDone
  assertBool "There is a company" $ isJust mcompany
  assertBool "User is company admin" $ useriscompanyadmin user
  assertEqual "Company name was saved on company" (Just cname) (getCompanyName <$> mcompany)
  assertEqual "First name was saved on user" fstname (getFirstName user)
  assertEqual "Last name was saved on user" sndname (getLastName user)
  assertEqual "Company position was saved on user" position (usercompanyposition $ userinfo user)
  assertEqual "Phone number was saved on user" phone (userphone $ userinfo user)

testChangeEmailAddress :: DBEnv -> Assertion
testChangeEmailAddress env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        , ("newemailagain", inText "jim@bob.com")
                        ]
  (res1, ctx1) <- runTestKontra req1 ctx $ handleUserPost >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res1)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res1))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx1)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx1) `isFlashOfType` OperationDone
  Just uuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email hasn't changed yet" "bob@blue.com" (getEmail uuser)

  actions <- getRequestChangeEmailActions
  assertEqual "A request change email action was made" 1 (length $ actions)
  let action = head actions
      (RequestEmailChange inviterid invitedemail token) = actionType action
  assertEqual "Inviter id is correct" (userid user) inviterid
  assertEqual "Action email is correct" (Email "jim@bob.com") invitedemail

  emails <- dbQuery GetEmails
  assertEqual "An email was sent" 1 (length emails)

  req2 <- mkRequest POST [("password", inText "abc123")]
  (res2, ctx2) <- runTestKontra req2 ctx1 $ handlePostChangeEmail (actionID action) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res2)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res2))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx2)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx2) `isFlashOfType` OperationDone
  Just uuuser <- dbQuery $ GetUserByID (userid user)
  assertEqual "Email has changed" "jim@bob.com" (getEmail uuuser)

testAddressesMustMatchToRequestEmailChange :: DBEnv -> Assertion
testAddressesMustMatchToRequestEmailChange env = withTestEnvironment env $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        , ("newemailagain", inText "jim2@bob.com")
                        ]
  (res1, ctx1) <- runTestKontra req1 ctx $ handleUserPost >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res1)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res1))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx1)
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx1) `isFlashOfType` OperationFailed

testNeedTwoAddressesToRequestEmailChange :: DBEnv -> Assertion
testNeedTwoAddressesToRequestEmailChange env = withTestEnvironment env $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        , ("newemailagain", inText "")
                        ]
  (res1, ctx1) <- runTestKontra req1 ctx $ handleUserPost >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res1)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res1))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx1)
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx1) `isFlashOfType` OperationFailed

testNeedEmailToBeUniqueToRequestChange :: DBEnv -> Assertion
testNeedEmailToBeUniqueToRequestChange env = withTestEnvironment env $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req1 <- mkRequest POST [ ("changeemail", inText "true")
                        , ("newemail", inText "jim@bob.com")
                        , ("newemailagain", inText "jim@bob.com")
                        ]
  (res1, ctx1) <- runTestKontra req1 ctx $ handleUserPost >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res1)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res1))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx1)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx1) `isFlashOfType` OperationDone
  actions <- getRequestChangeEmailActions
  assertEqual "No request email action was made" 0 (length $ actions)

testEmailChangeFailsIfActionIDIsWrong :: DBEnv -> Assertion
testEmailChangeFailsIfActionIDIsWrong env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "abc123")]
  action <- newRequestEmailChange user (Email "jim@bob.com")
  let (RequestEmailChange _inviterid _invitedemail token) = actionType action
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (ActionID 123) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfMagicHashIsWrong :: DBEnv -> Assertion
testEmailChangeFailsIfMagicHashIsWrong env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "abc123")]
  action <- newRequestEmailChange user (Email "jim@bob.com")
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (unsafeMagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeIfForAnotherUser :: DBEnv -> Assertion
testEmailChangeIfForAnotherUser env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  Just anotheruser <- addNewUser "Fred" "Frog" "fred@frog.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "abc123")]
  action <- newRequestEmailChange anotheruser (Email "jim@bob.com")
  let (RequestEmailChange _inviterid _invitedemail token) = actionType action
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfEmailInUse:: DBEnv -> Assertion
testEmailChangeFailsIfEmailInUse env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "abc123")]
  action <- newRequestEmailChange user (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (unsafeMagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfPasswordWrong :: DBEnv -> Assertion
testEmailChangeFailsIfPasswordWrong env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "wrongpassword")]
  action <- newRequestEmailChange user (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (unsafeMagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfNoPassword :: DBEnv -> Assertion
testEmailChangeFailsIfNoPassword env = withTestEnvironment env $ do
  Just user' <- addNewUser "Bob" "Blue" "bob@blue.com"
  passwordhash <- createPassword "abc123"
  _ <- dbUpdate $ SetUserPassword (userid user') passwordhash
  Just user <- dbQuery $ GetUserByID (userid user')
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [("password", inText "")]
  action <- newRequestEmailChange user (Email "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (unsafeMagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

getRequestChangeEmailActions :: MonadIO m => m [Action]
getRequestChangeEmailActions = do
  now <- getMinutesTime
  let expirytime = (7 * 24 * 60 + 1) `minutesAfter` now
  allactions <- query $ GetExpiredActions LeisureAction expirytime
  return $ filter isRequestChangeEmailAction allactions

isRequestChangeEmailAction :: Action -> Bool
isRequestChangeEmailAction action =
  case actionType action of
    (RequestEmailChange _ _ _) ->  True
    _ -> False
