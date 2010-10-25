module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import RememberMeTest
import SchedulerTest

main = defaultMain tests

tests = [testGroup "UserState" userStateTests,
         testGroup "RememberMe" rememberMeTests,
         testGroup "Scheduler" schedulerTests]
