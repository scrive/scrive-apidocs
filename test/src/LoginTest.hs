module LoginTest (loginTests) where

import Control.Applicative
import Data.List
import Happstack.Server
import Happstack.State
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Char8 as BS

import AppControl
import Context
import FlashMessage
import Redirect
import StateHelper
import Templates.TemplatesLoader
import TestKontra as T
import User.Password
import User.UserState
import Misc

loginTests :: Test
loginTests = testGroup "Login" [
      testCase "can login with valid user and password" testSuccessfulLogin
    , testCase "can't login with invalid user" testCantLoginWithInvalidUser
    , testCase "can't login with invalid password" testCantLoginWithInvalidPassword
    ]

testSuccessfulLogin :: Assertion
testSuccessfulLogin = withTestState $ do
    uid <- createTestUser
    ctx <- mkContext =<<  localizedVersion defaultValue <$> readGlobalTemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location is /" $ T.getHeader "location" (rsHeaders res) == Just "/"
    assertBool "User was logged into context" $ (userid <$> ctxmaybeuser ctx') == Just uid
    assertBool "No flash messages were added" $ null $ ctxflashmessages ctx'

testCantLoginWithInvalidUser :: Assertion
testCantLoginWithInvalidUser = withTestState $ do
    _ <- createTestUser
    ctx <- mkContext =<<  localizedVersion defaultValue <$> readGlobalTemplates
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

testCantLoginWithInvalidPassword :: Assertion
testCantLoginWithInvalidPassword = withTestState $ do
    _ <- createTestUser
    ctx <- mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'

loginFailureChecks :: Response -> Context -> Assertion
loginFailureChecks res ctx = do
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location starts with /?logging" $ (isPrefixOf "/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
    assertBool "User wasn't logged into context" $ ctxmaybeuser ctx == Nothing
    assertBool "One flash message was added" $ length (ctxflashmessages ctx) == 1
    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx) `isFlashOfType` OperationFailed

createTestUser :: IO UserID
createTestUser = do
    pwd <- createPassword $ BS.pack "admin"
    Just User{userid} <- update $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") pwd Nothing Nothing Nothing defaultValue
    return userid
