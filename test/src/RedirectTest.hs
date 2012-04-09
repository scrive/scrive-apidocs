{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module RedirectTest where

import Test.HUnit (Assertion)
import Test.Framework
import Redirect
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative
import Data.List
import DBError
import Misc
import StateHelper
import Templates.TemplatesLoader
import Happstack.Server
import Context
import KontraError
import TestingUtil
import TestKontra as T
import User.Locale
import qualified Control.Exception.Lifted as E

redirectTests :: Test
redirectTests = testGroup "RedirectTests"
                  [testCase "return Right if Right" testGuardRight
                  ,testCase "return mzero if Left" testStringGuardMZeroLeft
                  ,testCase "finishWith if Left DBNotLoggedIn" testDBErrorGuardRedirectLeftDBNotLoggedIn
                  ,testCase "mzero if Left DBResourceNotAvailable" testDBErrorGuardMZeroLeft
                  ]

testResponse :: String
testResponse = "hello"

testGuardRight :: Assertion
testGuardRight = withTestState $ do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (guardRight $ (Right testResponse :: Either String String))
    assertEqual "should be equal" res testResponse

testStringGuardMZeroLeft :: Assertion
testStringGuardMZeroLeft = withTestState $ do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (E.catch (guardRight $ (Left "error" :: Either String String))
                                                  (\(_::KontraError) -> return testResponse))
    assertEqual "should be equal" res testResponse

testDBErrorGuardRedirectLeftDBNotLoggedIn :: Assertion
testDBErrorGuardRedirectLeftDBNotLoggedIn = withTestState $ do
  globaltemplates <- readGlobalTemplates
  ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (res, ctx') <- runTestKontra req ctx (do
                                           _ <- guardRight $ Left DBNotLoggedIn
                                           ok $ toResponseBS "stuff" "hello")
  assertBool "Response code is 303" $ rsCode res == 303
  assertBool "Location starts with /se/sv/?logging" $ (isPrefixOf "/se/sv/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
  assertBool "One flash message was added" $ length (ctxflashmessages ctx') == 1
--    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testDBErrorGuardMZeroLeft :: Assertion
testDBErrorGuardMZeroLeft = withTestState $ do
  globaltemplates <- readGlobalTemplates
  ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (res, _ctx') <- runTestKontra req ctx (E.catch (guardRight $ (Left DBResourceNotAvailable))
                                                    (\(_::KontraError) -> return testResponse))
  assertEqual "Should be equal" res testResponse
