module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import SchedulerTest
import AppViewSampler
import UserViewSampler
import DocViewMailSampler
import DocViewSampler
import DocControlTest
import TrustWeaverTest
import InputValidationTest
import System.IO

main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests = [testGroup "UserState" userStateTests,
         testGroup "Scheduler" schedulerTests,
         testGroup "AppView (Sampler)" appViewSamples,
         testGroup "UserView (Sampler)" userViewSamples,
         testGroup "DocViewMail (Sampler)" docViewMailSamples,
         testGroup "DocView (Sampler)" docViewSamples,
         testGroup "DocControl" docControlTests,
         testGroup "TrustWeaver" trustWeaverTests,
         testGroup "InputValidation" inputValidationTests
        ]
