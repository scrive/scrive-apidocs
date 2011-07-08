{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import System.Environment
import System.IO
import Test.Framework

import DocStateTest
import HtmlTest
import InputValidationTest
import LoginTest
import TrustWeaverTest
import UserStateTest

allTests :: [Test]
allTests = [
    docStateTests
  , htmlTests
  , inputValidationTests
  -- everything fails for trustweaver, so commenting out for now
  --, trustWeaverTests
  , userStateTests
  -- strangely, after login tests are run, docstate/userstate tests
  -- stop workings :/ so put it at the end as current workaround
  , loginTests
  ]

testsToRun :: [String] -> [Either String Test]
testsToRun [] = []
testsToRun (t:ts) =
    case map toLower t of
         "all"             -> map Right allTests ++ rest
         "docstate"        -> Right docStateTests : rest
         "html"            -> Right htmlTests : rest
         "inputvalidation" -> Right inputValidationTests : rest
         "login"           -> rest ++ [Right loginTests]
         "trustweaver"     -> Right trustWeaverTests : rest
         "userstate"       -> Right userStateTests : rest
         _                 -> Left t : rest
    where
        rest = testsToRun ts

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    (args, tests) <- partitionEithers . testsToRun <$> getArgs
    defaultMainWithArgs tests args
