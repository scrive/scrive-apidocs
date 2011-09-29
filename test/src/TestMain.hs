{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import Database.HDBC.PostgreSQL
import System.Environment.UTF8
import System.IO
import Test.Framework

import DB.Classes
import DB.Migrations

-- Note: if you add new testsuites here, please add them in a similar
-- manner to existing ones, i.e. wrap them around ifdefs and add appropriate
-- flags to kontrakcja.cabal to allow possibility of disabling tests selectively
-- if e.g. for some reason they stop compiling. Also, please keep them in
-- alphabetic order.

#ifndef NO_COMPANYSTATE
import CompanyStateTest
#endif
#ifndef NO_DOCSTATE
import DocStateTest
#endif
#ifndef NO_DOCCONTROL
import DocControlTest
#endif
#ifndef NO_DOCSTATEQUERY
import DocStateQueryTest
#endif
#ifndef NO_HTML
import HtmlTest
#endif
#ifndef NO_INPUTVALIDATION
import InputValidationTest
#endif
#ifndef NO_INTEGRATIONAPI
import IntegrationAPITest
#endif
#ifndef NO_LOGIN
import LoginTest
#endif
#ifndef NO_MAILAPI
import MailAPITest
#endif
#ifndef NO_REDIRECT
import RedirectTest
#endif
#ifndef NO_SERVICESTATE
import ServiceStateTest
#endif
#ifndef NO_TRUSTWEAVER
import TrustWeaverTest
#endif
#ifndef NO_USERSTATE
import UserStateTest
#endif
#ifndef NO_CSVUTIL
import CSVUtilTest
#endif
#ifndef NO_SIMPLEEMAIL
import SimpleMailTest
#endif
#ifndef NO_LOCALE
--import LocaleTest
#endif

allTests :: Connection -> [Test]
allTests conn = tail tests
  where
    tests = [
        undefined
#ifndef NO_COMPANYSTATE
      , companyStateTests conn
#endif
#ifndef NO_DOCSTATE
      , docStateTests conn
#endif
#ifndef NO_DOCCONTROL
      , docControlTests conn
#endif
#ifndef NO_DOCSTATEQUERY
      , docStateQueryTests
#endif
#ifndef NO_HTML
      , htmlTests
#endif
#ifndef NO_INPUTVALIDATION
      , inputValidationTests
#endif
#ifndef NO_INTEGRATIONAPI
      , integrationAPITests conn
#endif
#ifndef NO_LOGIN
      , loginTests conn
#endif
#ifndef NO_MAILAPI
      , mailApiTests conn
#endif
#ifndef NO_REDIRECT
      , redirectTests
#endif
#ifndef NO_SERVICESTATE
      , serviceStateTests conn
#endif
#ifndef NO_TRUSTWEAVER
      -- everything fails for trustweaver, so commenting out for now
      --, trustWeaverTest
#endif
#ifndef NO_USERSTATE
      , userStateTests conn
#endif
#ifndef NO_CSVUTIL
      , csvUtilTests
#endif
#ifndef NO_SIMPLEEMAIL
      , simpleMailTests 
#endif
#ifndef NO_LOCALE
--      , localeTests conn
#endif
      ]

testsToRun :: Connection -> [String] -> [Either String Test]
testsToRun _ [] = []
testsToRun conn (t:ts) =
  case map toLower t of
    "all"             -> map Right (allTests conn) ++ rest
#ifndef NO_COMPANYSTATE
    "companystate"    -> Right (companyStateTests conn) : rest
#endif
#ifndef NO_DOCSTATE
    "docstate"        -> Right (docStateTests conn) : rest
#endif
#ifndef NO_DOCCONTROL
    "doccontrol"        -> Right (docControlTests conn) : rest
#endif
#ifndef NO_DOCSTATEQUERY
    "docstatequery"   -> Right docStateQueryTests : rest
#endif
#ifndef NO_HTML
    "html"            -> Right htmlTests : rest
#endif
#ifndef NO_INPUTVALIDATION
    "inputvalidation" -> Right inputValidationTests : rest
#endif
#ifndef NO_INTEGRATIONAPI
    "integrationapi"  -> Right (integrationAPITests conn) : rest
#endif
#ifndef NO_LOGIN
    "login"           -> Right (loginTests conn) : rest
#endif
#ifndef NO_MAILAPI
    "mailapi"         -> Right (mailApiTests conn) : rest
#endif
#ifndef NO_REDIRECT
    "redirect"        -> Right redirectTests : rest
#endif
#ifndef NO_SERVICESTATE
    "servicestate"    -> Right (serviceStateTests conn) : rest
#endif
#ifndef NO_TRUSTWEAVER
    "trustweaver"     -> Right trustWeaverTests : rest
#endif
#ifndef NO_USERSTATE
    "userstate"       -> Right (userStateTests conn) : rest
#endif
#ifndef NO_CSVUTIL
    "csvutil"         -> Right csvUtilTests : rest
#endif
#ifndef NO_SIMPLEMAIL  
    "simplemail"      -> Right simpleMailTests : rest
#endif
#ifndef NO_LOCALE
--    "locale"          -> Right (localeTests conn) : rest
#endif
    _                 -> Left t : rest
  where
    rest = testsToRun conn ts

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- readFile "kontrakcja_test.conf"
  withPostgreSQL pgconf $ \conn -> do
    ioRunDB conn checkDBConsistency
    (args, tests) <- partitionEithers . testsToRun conn <$> getArgs
    defaultMainWithArgs tests args
