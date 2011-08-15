module LoginTest (loginTests) where

import Control.Applicative
import Data.List
import Database.HDBC.PostgreSQL
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.Char8 as BS

import AppControl
import DB.Classes
import Context
import FlashMessage
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import Misc

loginTests :: Connection -> Test
loginTests conn = testGroup "Login" [
      testCase "can login with valid user and password" $ testSuccessfulLogin conn
    , testCase "can't login with invalid user" $ testCantLoginWithInvalidUser conn
    , testCase "can't login with invalid password" $ testCantLoginWithInvalidPassword conn
    ]

testSuccessfulLogin :: Connection -> Assertion
testSuccessfulLogin conn = withTestEnvironment conn $ do
    uid <- createTestUser
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location is /" $ T.getHeader "location" (rsHeaders res) == Just "/"
    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testCantLoginWithInvalidUser :: Connection -> Assertion
testCantLoginWithInvalidUser conn = withTestEnvironment conn $ do
    _ <- createTestUser
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: Connection -> Assertion
testCantLoginWithInvalidPassword conn = withTestEnvironment conn $ do
    _ <- createTestUser
    ctx <- (\c -> c { ctxdbconn = conn })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

loginFailureChecks :: Response -> Context -> DB ()
loginFailureChecks res ctx = do
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location starts with /?logging" $ (isPrefixOf "/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
    assertBool "User wasn't logged into context" $ ctxmaybeuser ctx == Nothing
    assertBool "One flash message was added" $ length (ctxflashmessages ctx) == 1
    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx) `isFlashOfType` OperationFailed

createTestUser :: DB UserID
createTestUser = do
    pwd <- createPassword $ BS.pack "admin"
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") (Just pwd) False Nothing Nothing defaultValue
    return userid
