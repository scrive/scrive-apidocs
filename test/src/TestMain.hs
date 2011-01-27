module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import SchedulerTest
import UserViewSampler
import DocViewMailSampler
import DocViewSampler
import DocControlTest
import TrustWeaverTest
import System.IO

main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests = [testGroup "UserState" userStateTests,
         testGroup "Scheduler" schedulerTests,
         testGroup "UserView (Sampler)" userViewSamples,
         testGroup "DocViewMail (Sampler)" docViewMailSamples,
         testGroup "DocView (Sampler)" docViewSamples,
         testGroup "DocControl" docControlTests,
         testGroup "TrustWeaver" trustWeaverTests
        ]
