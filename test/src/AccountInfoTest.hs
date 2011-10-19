module AccountInfoTest (accountInfoTests) where

import Control.Applicative
import Control.Monad.State
import Database.HDBC.PostgreSQL
import Data.Maybe
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.UTF8 as BS

import Context
import DB.Classes
import FlashMessage
import Misc
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import User.UserControl
import User.Utils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

accountInfoTests :: Connection -> Test
accountInfoTests conn = testGroup "AccountInfo" [
      testCase "lets private account upgrade to company account" $ testPrivateToCompanyUpgrade conn
    , testCase "company upgrade requires a company name" $ testCompanyUpgradeRequiresCompanyName conn
    , testCase "company upgrade requires a company first name" $ testCompanyUpgradeRequiresFirstName conn
    , testCase "company upgrade requires a company second name" $ testCompanyUpgradeRequiresSecondName conn
    , testCase "company upgrade requires a company position" $ testCompanyUpgradeRequiresPosition conn
    , testCase "company upgrade requires a phone number" $ testCompanyUpgradeRequiresPhone conn
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

testCompanyUpgradeRequiresFirstName :: Connection -> Assertion
testCompanyUpgradeRequiresFirstName conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                ""
                                "Rybczakk"
                                "Tester"
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeFailed (userid user) (res1, ctx1)

testCompanyUpgradeRequiresSecondName :: Connection -> Assertion
testCompanyUpgradeRequiresSecondName conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                "Andrzejj"
                                ""
                                "Tester"
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeFailed (userid user) (res1, ctx1)

testCompanyUpgradeRequiresPosition :: Connection -> Assertion
testCompanyUpgradeRequiresPosition conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                "Andrzejj"
                                "Rybczakk"
                                ""
                                "12345"
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeFailed (userid user) (res1, ctx1)

testCompanyUpgradeRequiresPhone :: Connection -> Assertion
testCompanyUpgradeRequiresPhone conn = withTestEnvironment conn $ do
  Just user <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let upgradeinfo = UpgradeInfo "Test Corp"
                                "Andrzejj"
                                "Rybczakk"
                                "Tester"
                                ""
  (res1, ctx1) <- upgradeCompanyForUser conn user upgradeinfo

  assertCompanyUpgradeFailed (userid user) (res1, ctx1)

data UpgradeInfo = UpgradeInfo String String String String String

upgradeCompanyForUser :: (MonadIO m, Functor m) =>
                               Connection
                            -> User
                            -> UpgradeInfo
                            -> m (Response, Context)
upgradeCompanyForUser conn user (UpgradeInfo cname fstname sndname position phone) = do
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)

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
  assertEqual "Location is /account" (Just "/account") (T.getHeader "location" (rsHeaders res))
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