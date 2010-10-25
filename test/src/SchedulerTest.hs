module SchedulerTest(
    schedulerTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import StateHelper
import UserState

import Happstack.State (update, query)

import MinutesTime
import Scheduler
import DocState

schedulerTests :: [Test]
schedulerTests = [testGroup "Scheduler" 
                     [testCase "Checks if after scheduler run there are no timedout documents" testDocumentsBecameTimeouted]  
                 ] 


testDocumentsBecameTimeouted = withTestState $ 
            do
             now <- getMinutesTime
             runScheduler
             docs <- query $ GetTimeoutedButPendingDocuments now
             assert $ null docs
