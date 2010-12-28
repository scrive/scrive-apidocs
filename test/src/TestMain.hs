module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import SchedulerTest
import UserViewSampler
import DocViewMailSampler
import DocViewSampler
import TrustWeaverTest

main = defaultMain tests

tests = [testGroup "UserState" userStateTests,
         testGroup "Scheduler" schedulerTests,
         testGroup "UserView (Sampler)" userViewSamples,
         testGroup "DocViewMail (Sampler)" docViewMailSamples,
         testGroup "DocView (Sampler)" docViewSamples,
         testGroup "TrustWeaver" trustWeaverTests
        ]
