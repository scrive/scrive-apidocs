module LoginTest (loginTests) where

import Network.HTTP
import Network.URI
import Test.HUnit (Assertion)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import HttpHelper

loginTests :: Test
loginTests = testGroup "Login"
    [ testGroup "https"
        [ testCase "can login with valid user and password" testSuccessfulLogin,
          testCase "can't login with invalid user" testCantLoginWithInvalidUser,
          testCase "can't login with invalid password" testCantLoginWithInvalidPassword ]
    ]

testSuccessfulLogin :: Assertion
testSuccessfulLogin = withTestServer $ do
  rsp <- postLoginForm "lukas@skrivapa.se" "admin"
  assertURL "/" rsp
  assertXPathDoesntExist loginForm rsp

testCantLoginWithInvalidUser :: Assertion
testCantLoginWithInvalidUser = withTestServer $ do
  rsp <- postLoginForm "noone@skrivapa.se" "admin"
  assertURLStartsWith "/?logging" rsp
  assertXPathExists loginForm rsp

testCantLoginWithInvalidPassword :: Assertion
testCantLoginWithInvalidPassword = withTestServer $ do
  rsp <- postLoginForm "lukas@skrivapa.se" "wrongpassword"
  assertURLStartsWith "/?logging" rsp
  assertXPathExists loginForm rsp
  
loginForm :: String
loginForm = "//form[@id='loginForm']"

type LoginEmail = String
type LoginPassword = String

postLoginForm :: LoginEmail -> LoginPassword -> IO (URI, Response String)
postLoginForm loginemail loginpassword =
  postForm "/login" [("email", loginemail), ("password", loginpassword)]

