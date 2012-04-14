{-# LANGUAGE OverloadedStrings #-}
module RedirectTest where

import Test.Framework
import Redirect
import Control.Applicative
import Data.List
import Crypto.RNG
import DB
import DBError
import Misc
import Templates.TemplatesLoader
import Happstack.Server
import Context
import KontraError
import TestingUtil
import TestKontra as T
import User.Locale
import qualified Control.Exception.Lifted as E

redirectTests :: (Nexus, CryptoRNGState) -> Test
redirectTests env = testGroup "RedirectTests" [
    testThat "return Right if Right" env testGuardRight
  , testThat "return mzero if Left" env testStringGuardMZeroLeft
  , testThat "finishWith if Left DBNotLoggedIn" env testDBErrorGuardRedirectLeftDBNotLoggedIn
  , testThat "mzero if Left DBResourceNotAvailable" env testDBErrorGuardMZeroLeft
  ]

testResponse :: String
testResponse = "hello"

testGuardRight :: TestEnv ()
testGuardRight = do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (guardRight $ (Right testResponse :: Either String String))
    assertEqual "should be equal" res testResponse

testStringGuardMZeroLeft :: TestEnv ()
testStringGuardMZeroLeft =  do
    globaltemplates <- readGlobalTemplates
    ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (E.catch (guardRight $ (Left "error" :: Either String String))
                                                  (\(_::KontraError) -> return testResponse))
    assertEqual "should be equal" res testResponse

testDBErrorGuardRedirectLeftDBNotLoggedIn :: TestEnv ()
testDBErrorGuardRedirectLeftDBNotLoggedIn = do
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

testDBErrorGuardMZeroLeft :: TestEnv ()
testDBErrorGuardMZeroLeft = do
  globaltemplates <- readGlobalTemplates
  ctx <- mkContext (mkLocaleFromRegion defaultValue) globaltemplates
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (res, _ctx') <- runTestKontra req ctx (E.catch (guardRight $ (Left DBResourceNotAvailable))
                                                    (\(_::KontraError) -> return testResponse))
  assertEqual "Should be equal" res testResponse
