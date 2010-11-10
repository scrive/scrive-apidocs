module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import SchedulerTest

main = defaultMain tests

tests = [testGroup "UserState" userStateTests,
         testGroup "Scheduler" schedulerTests]
