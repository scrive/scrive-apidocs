{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module RedirectTest where

import Test.HUnit (assertFailure, Assertion, assertEqual, assertBool)
import Test.Framework
import TestingUtil
import Redirect
import Test.Framework.Providers.HUnit (testCase)
import Control.Monad
--import Control.Monad.Trans
import Control.Applicative
import Data.List
import DBError
import TestKontra
import StateHelper
import Templates.TemplatesLoader
import Happstack.Server
import Context
import TestKontra as T

redirectTests :: Test
redirectTests = testGroup "RedirectTests" 
                  [testCase "return Right if Right" testGuardRight
                  ,testCase "return mzero if Left" testStringGuardMZeroLeft
                  ,testCase "finishWith if Left DBNotLoggedIn" testDBErrorGuardRedirectLeftDBNotLoggedIn
                     ]
                  
testGuardRight :: Assertion
testGuardRight = withTestState $ do
    ctx <- mkContext =<< readTemplates LANG_SE
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (guardRight $ (Right ("hello" :: String) :: Either String String))
    assertEqual "should be equal" res "hello"

testStringGuardMZeroLeft :: Assertion
testStringGuardMZeroLeft = withTestState $ do
    ctx <- mkContext =<< readTemplates LANG_SE
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (mplus (guardRight $ (Left ("hello" :: String) :: Either String String))
                                       (return "hello"))
    assertEqual "should be equal" res "hello"


testDBErrorGuardRedirectLeftDBNotLoggedIn :: Assertion
testDBErrorGuardRedirectLeftDBNotLoggedIn = withTestState $ do
    ctx <- mkContext =<< readTemplates LANG_SE
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx (do
                                          _ <- guardRight $ Left DBNotLoggedIn
                                          ok $ toResponseBS "stuff" "hello")
    assertBool "Response code is 303" $ rsCode res == 303
    assertBool "Location starts with /?logging" $ (isPrefixOf "/?logging" <$> T.getHeader "location" (rsHeaders res)) == Just True
    assertBool "One flash message was added" $ length (ctxflashmessages ctx') == 1
--    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed
                

assertMZero :: IO t -> IO ()
assertMZero action = 
  mplus 
    (do
        _ <- action
        assertFailure "did not mzero")
    assertSuccess

