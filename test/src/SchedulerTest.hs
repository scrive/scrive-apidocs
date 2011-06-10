{-# LANGUAGE CPP #-}
module SchedulerTest  where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)

import StateHelper
import User.UserState

import Happstack.State (update, query)

import MinutesTime
import ActionScheduler
import Doc.DocState
import AppControl
import System.IO

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    defaultMain tests

tests :: [Test]
tests = [ testGroup "ActionScheduler" 
                    []
           -- [testCase "Checks if after scheduler run there are no timedout documents" testDocumentsBecameTimeouted]  
        ] 

{-
testDocumentsBecameTimeouted = withTestState $ 
            do
             now <- getMinutesTime
             runScheduler $ AppConf { httpPort = 8000
                             , store    = ""
                             , static   = ""
                             , awsBucket = ""
                             , awsSecretKey = ""
                             , awsAccessKey = ""
                             , production = False
                             }
             docs <- query $ GetTimeoutedButPendingDocuments now
             assert $ null docs
-}
