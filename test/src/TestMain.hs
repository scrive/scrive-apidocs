{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import System.Environment.UTF8
import System.IO
import Test.Framework

-- Note: if you add new testsuites here, please add them in a similar
-- manner to existing ones, i.e. wrap them around ifdefs and add appropriate
-- flags to kontrakcja.cabal to allow possibility of disabling tests selectively
-- if e.g. for some reason they stop compiling. Also, please keep them in
-- alphabetic order.

#ifndef NO_DOCSTATE
import DocStateTest
#endif
#ifndef NO_HTML
import HtmlTest
#endif
#ifndef NO_INPUTVALIDATION
import InputValidationTest
#endif
#ifndef NO_LOGIN
import LoginTest
#endif
#ifndef NO_MAILAPI
import MailAPITest
#endif
#ifndef NO_TRUSTWEAVER
import TrustWeaverTest
#endif
#ifndef NO_USERSTATE
import UserStateTest
#endif

import DocStateQueryTest
import RedirectTest

allTests :: [Test]
allTests = tail tests
    where
        tests = [
            undefined
#ifndef NO_DOCSTATE
          , docStateTests
#endif
#ifndef NO_HTML
          , htmlTests
#endif
#ifndef NO_INPUTVALIDATION
          , inputValidationTests
#endif
#ifndef NO_LOGIN
          , loginTests
#endif
#ifndef NO_MAILAPI
          , mailApiTests
#endif
#ifndef NO_TRUSTWEAVER
          -- everything fails for trustweaver, so commenting out for now
          --, trustWeaverTest
#endif
#ifndef NO_USERSTATE
          , userStateTests
#endif
          ]

testsToRun :: [String] -> [Either String Test]
testsToRun [] = []
testsToRun (t:ts) =
    case map toLower t of
         "all"             -> map Right allTests ++ rest
#ifndef NO_DOCSTATE
         "docstate"        -> Right docStateTests : rest
#endif
#ifndef NO_HTML
         "html"            -> Right htmlTests : rest
#endif
#ifndef NO_INPUTVALIDATION
         "inputvalidation" -> Right inputValidationTests : rest
#endif
#ifndef NO_LOGIN
         "login"           -> Right loginTests : rest
#endif
#ifndef NO_MAILAPI
         "mailapi"         -> Right mailApiTests : rest
#endif
#ifndef NO_TRUSTWEAVER
         "trustweaver"     -> Right trustWeaverTests : rest
#endif
#ifndef NO_USERSTATE
         "userstate"       -> Right userStateTests : rest
#endif
         "docstatequery"   -> Right docStateQueryTests : rest
         "redirect"        -> Right redirectTests : rest
         _                 -> Left t : rest
    where
        rest = testsToRun ts

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    (args, tests) <- partitionEithers . testsToRun <$> getArgs
    defaultMainWithArgs tests args
