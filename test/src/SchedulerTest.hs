module SchedulerTest(
    schedulerTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import StateHelper
import User.UserState

import Happstack.State (update, query)

import MinutesTime
import Scheduler
import Doc.DocState
import AppControl
import Happstack.Server

schedulerTests :: [Test]
schedulerTests = [testGroup "Scheduler" 
                     [testCase "Checks if after scheduler run there are no timedout documents" testDocumentsBecameTimeouted]  
                 ] 


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
