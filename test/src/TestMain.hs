{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import System.Environment.UTF8
import System.IO
import Test.Framework

import DocStateTest
import HtmlTest
import InputValidationTest
import LoginTest
import MailAPITest
import TrustWeaverTest
import UserStateTest

allTests :: [Test]
allTests = [
    docStateTests
  , htmlTests
  , inputValidationTests
  , loginTests
  , mailApiTests
  -- everything fails for trustweaver, so commenting out for now
  --, trustWeaverTests
  , userStateTests
  ]

testsToRun :: [String] -> [Either String Test]
testsToRun [] = []
testsToRun (t:ts) =
    case map toLower t of
         "all"             -> map Right allTests ++ rest
         "docstate"        -> Right docStateTests : rest
         "html"            -> Right htmlTests : rest
         "inputvalidation" -> Right inputValidationTests : rest
         "login"           -> Right loginTests : rest
         "mailapi"         -> Right mailApiTests : rest
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
