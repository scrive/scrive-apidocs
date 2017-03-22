{-# LANGUAGE OverloadedStrings #-}
module Monitor.APITest (monitorAPITests) where

import Data.Aeson
import Happstack.Server
import Test.Framework
import qualified Data.HashMap.Strict as H

import DB
import Doc.SignatoryLinkID ()
import KontraPrelude
import Monitor.API
import TestingUtil
import TestKontra as T

monitorAPITests :: TestEnvSt -> Test
monitorAPITests env = testGroup "MonitorAPI"
  [ testThat "Test Monitor API Get Status" env testMonitorStatusGet
  ]

testMonitorStatusGet :: TestEnv ()
testMonitorStatusGet = do
  ctx <- mkContext def
  req1 <- mkRequest GET []
  (res1,_) <- runTestKontra req1 ctx $ apiCallMonitorStatusGet
  let Just (Object resObject1) = decode (rsBody res1) :: Maybe Value
      Just (String status) = H.lookup "status" resObject1
  assertEqual "We should get a 200 response" 200 (rsCode res1)
  assertEqual "We should get status 'ok'" "ok" status
  return ()
