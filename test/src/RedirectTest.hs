module RedirectTest where

import Test.Framework
import Redirect
import Control.Applicative
import Data.List
import DBError
import Utils.Default
import Happstack.Server
import Context
import KontraError
import TestingUtil
import TestKontra as T
import User.Lang()
import qualified Control.Exception.Lifted as E

redirectTests :: TestEnvSt -> Test
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
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (guardRight $ (Right testResponse :: Either String String))
    assertEqual "should be equal" res testResponse

testStringGuardMZeroLeft :: TestEnv ()
testStringGuardMZeroLeft =  do
    ctx <- mkContext defaultValue
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    (res, _) <- runTestKontra req ctx (E.catch (guardRight $ (Left "error" :: Either String String))
                                                  (\(_::KontraError) -> return testResponse))
    assertEqual "should be equal" res testResponse

testDBErrorGuardRedirectLeftDBNotLoggedIn :: TestEnv ()
testDBErrorGuardRedirectLeftDBNotLoggedIn = do
  ctx <- mkContext defaultValue
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (res, ctx') <- runTestKontra req ctx (do
                                           _ <- guardRight $ Left DBNotLoggedIn
                                           ok $ toResponseBS "stuff" "hello")
  assertBool "Response code is 303" $ rsCode res == 303
  assertBool "Location starts with /login" $ (isInfixOf "login" <$> T.getHeader "location" (rsHeaders res)) == Just True
  assertBool "One flash message was added" $ length (ctxflashmessages ctx') == 1
--    assertBool "Flash message has type indicating failure" $ head (ctxflashmessages ctx') `isFlashOfType` OperationFailed

testDBErrorGuardMZeroLeft :: TestEnv ()
testDBErrorGuardMZeroLeft = do
  ctx <- mkContext defaultValue
  req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
  (res, _ctx') <- runTestKontra req ctx (E.catch (guardRight $ (Left DBResourceNotAvailable))
                                                    (\(_::KontraError) -> return testResponse))
  assertEqual "Should be equal" res testResponse
