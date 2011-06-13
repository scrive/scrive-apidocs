module Main where

import Test.Framework (defaultMain, testGroup)
import UserStateTest
import TrustWeaverTest hiding (tests,main)
import InputValidationTest hiding (tests,main)
import HtmlTest hiding (tests,main)
import System.IO

main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

tests = [testGroup "UserState" userStateTests,
         -- testGroup "TrustWeaver" trustWeaverTests,
         testGroup "InputValidation" inputValidationTests,
         testGroup "Html" htmlTests
        ]
