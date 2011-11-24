module AccountInfoTest (accountInfoTests) where

import Control.Applicative
import Control.Monad.State
import Database.HDBC.PostgreSQL
import Data.Maybe
import Happstack.Server
import Happstack.State (query)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.UTF8 as BS

import ActionSchedulerState
import Context
import DB.Classes
import DB.Types
import FlashMessage
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
import Util.MonadUtils

accountInfoTests :: Connection -> Test
accountInfoTests conn = testGroup "AccountInfo" [
      testCase "lets private account upgrade to company account" $ testPrivateToCompanyUpgrade conn
    , testCase "company upgrade requires a company name" $ testCompanyUpgradeRequiresCompanyName conn
    , testCase "lets users change their email addresses" $ testChangeEmailAddress conn
    , testCase  "requesting email change fails is the entered emails are mismatched" $ testAddressesMustMatchToRequestEmailChange conn
    , testCase "need two addresses to request email changes" $ testNeedTwoAddressesToRequestEmailChange conn
    , testCase "need unique email to request email change" $ testNeedEmailToBeUniqueToRequestChange conn
    , testCase "need the correct action id to complete email change" $ testEmailChangeFailsIfActionIDIsWrong conn
    , testCase "need the correct hash to complete email change" $ testEmailChangeFailsIfMagicHashIsWrong conn
    , testCase "need the correct user to complete email change" $ testEmailChangeIfForAnotherUser conn
    , testCase "need the email to still be unique to complete email change" $ testEmailChangeFailsIfEmailInUse conn
    ]

testPrivateToCompanyUpgrade :: Connection -> Assertion
testPrivateToCompanyUpgrade conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                "Andrzejj"
                                "Rybczakk"
                                "Tester"
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeSuccessful (userid user) upgradeinfo (res1, ctx1)

testCompanyUpgradeRequiresCompanyName :: Connection -> Assertion
testCompanyUpgradeRequiresCompanyName conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo ""
                                "Andrzejj"
                                "Rybczakk"
                                "Tester"
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeFailed (userid user) (res1, ctx1)

data UpgradeInfo = UpgradeInfo String String String String String

upgradeCompanyForUser :: (MonadIO m, Functor m) =>
                               Connection
                            -> User
                            -> UpgradeInfo
                            -> m (Response, Context)
upgradeCompanyForUser conn user (UpgradeInfo cname fstname sndname position phone) = do
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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
  mcompany <- getCompanyForUser user

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /account/companyaccounts" (Just "/account/companyaccounts?page=1") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is logged in" (Just $ userid user) (fmap userid $ ctxmaybeuser ctx)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx) `isFlashOfType` OperationDone
  assertBool "There is a company" $ isJust mcompany
  assertBool "User is company admin" $ useriscompanyadmin user
  assertEqual "Company name was saved on company" (Just cname) (fmap (BS.toString .getCompanyName) mcompany)
  assertEqual "First name was saved on user" fstname (BS.toString $ getFirstName user)
  assertEqual "Last name was saved on user" sndname (BS.toString $ getLastName user)
  assertEqual "Company position was saved on user" position (BS.toString . usercompanyposition $ userinfo user)
  assertEqual "Phone number was saved on user" phone (BS.toString . userphone $ userinfo user)

assertCompanyUpgradeFailed :: UserID -> (Response, Context) -> DB ()
assertCompanyUpgradeFailed uid (res, ctx) = do
  Just user <- dbQuery $ GetUserByID uid
  mcompany <- getCompanyForUser user

  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "Location is /account/?createcompany" (Just "/account/?createcompany") (T.getHeader "location" (rsHeaders res))
  assertEqual "User is logged in" (Just uid) (fmap userid $ ctxmaybeuser ctx)
  assertEqual "Flash message was added" 1 (length $ ctxflashmessages ctx)
  assertBool "Flash has type indicating a failure" $ head (ctxflashmessages ctx) `isFlashOfType` OperationFailed
  assertBool "User isn't a company admin" (not $ useriscompanyadmin user)
  assertEqual "There is no company" Nothing mcompany

testChangeEmailAddress :: Connection -> Assertion
testChangeEmailAddress conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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
  uuser <- guardJustM $ runDBQuery $ GetUserByID (userid user)
  assertEqual "Email hasn't changed yet" (BS.fromString "bob@blue.com") (getEmail uuser)

  actions <- getRequestChangeEmailActions
  assertEqual "A request change email action was made" 1 (length $ actions)
  let action = head actions
      (RequestEmailChange inviterid invitedemail token) = actionType action
  assertEqual "Inviter id is correct" (userid user) inviterid
  assertEqual "Action email is correct" (Email $ BS.fromString "jim@bob.com") invitedemail

  emailactions <- getEmailActions
  assertEqual "An email was sent" 1 (length emailactions)

  req2 <- mkRequest POST []
  (res2, ctx2) <- runTestKontra req2 ctx1 $ handlePostChangeEmail (actionID action) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res2)
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res2))
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx2)
  assertBool "Flash message has type indicating success" $ head (ctxflashmessages ctx2) `isFlashOfType` OperationDone
  uuuser <- guardJustM $ runDBQuery $ GetUserByID (userid user)
  assertEqual "Email has changed" (BS.fromString "jim@bob.com") (getEmail uuuser)

testAddressesMustMatchToRequestEmailChange :: Connection -> Assertion
testAddressesMustMatchToRequestEmailChange conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

testNeedTwoAddressesToRequestEmailChange :: Connection -> Assertion
testNeedTwoAddressesToRequestEmailChange conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

testNeedEmailToBeUniqueToRequestChange :: Connection -> Assertion
testNeedEmailToBeUniqueToRequestChange conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
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

testEmailChangeFailsIfActionIDIsWrong :: Connection -> Assertion
testEmailChangeFailsIfActionIDIsWrong conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  action <- liftIO $ newRequestEmailChange user (Email $ BS.fromString "jim@bob.com")
  let (RequestEmailChange _inviterid _invitedemail token) = actionType action
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (ActionID 123) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfMagicHashIsWrong :: Connection -> Assertion
testEmailChangeFailsIfMagicHashIsWrong conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  action <- liftIO $ newRequestEmailChange user (Email $ BS.fromString "jim@bob.com")
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (MagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeIfForAnotherUser :: Connection -> Assertion
testEmailChangeIfForAnotherUser conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  Just anotheruser <- addNewUser "Fred" "Frog" "fred@frog.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  action <- liftIO $ newRequestEmailChange anotheruser (Email $ BS.fromString "jim@bob.com")
  let (RequestEmailChange _inviterid _invitedemail token) = actionType action
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) token >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testEmailChangeFailsIfEmailInUse:: Connection -> Assertion
testEmailChangeFailsIfEmailInUse conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST []
  action <- liftIO $ newRequestEmailChange user (Email $ BS.fromString "jim@bob.com")

  Just _ <- addNewUser "Jim" "Bob" "jim@bob.com"
  (res, ctx') <- runTestKontra req ctx $ handlePostChangeEmail (actionID action) (MagicHash 123) >>= sendRedirect
  assertEqual "Response code is 303" 303 (rsCode res)
  assertEqual "A flash message was added" 1 (length $ ctxflashmessages ctx')
  assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

getEmailActions :: MonadIO m => m [Action]
getEmailActions = do
  now <- getMinutesTime
  let expirytime = 1 `minutesAfter` now
  allactions <- query $ GetExpiredActions EmailSendoutAction expirytime
  return $ filter isEmailAction allactions

isEmailAction :: Action -> Bool
isEmailAction action =
  case actionType action of
    (EmailSendout _) -> True
    _ -> False

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
