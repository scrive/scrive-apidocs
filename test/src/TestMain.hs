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
--import TrustWeaverTest
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

allTests :: Connection -> [(String,Test)]
allTests conn = tail tests
  where
    tests = [
        undefined
#ifndef NO_COMPANYSTATE
      , ("companystate", companyStateTests conn)
#endif
#ifndef NO_DOCSTATE
      , ("docstate", docStateTests conn)
#endif
#ifndef NO_DOCCONTROL
      , ("doccontrol", docControlTests conn)
#endif
#ifndef NO_DOCSTATEQUERY
      , ("docstatequery", docStateQueryTests)
#endif
#ifndef NO_HTML
      , ("html", htmlTests)
#endif
#ifndef NO_INPUTVALIDATION
      , ("inputvalidation", inputValidationTests)
#endif
#ifndef NO_INTEGRATIONAPI
      , ("integrationapi", integrationAPITests conn)
#endif
#ifndef NO_LOGIN
      , ("login", loginTests conn)
#endif
#ifndef NO_MAILAPI
      , ("mailapi", mailApiTests conn)
#endif
#ifndef NO_REDIRECT
      , ("redirect", redirectTests)
#endif
#ifndef NO_SERVICESTATE
      , ("servicestate", serviceStateTests conn)
#endif
#ifndef NO_TRUSTWEAVER
      -- everything fails for trustweaver, so commenting out for now
      --("trustweaver", trustWeaverTest)
#endif
#ifndef NO_USERSTATE
      , ("userstate", userStateTests conn)
#endif
#ifndef NO_CSVUTIL
      , ("csvutil", csvUtilTests)
#endif
#ifndef NO_SIMPLEEMAIL
      , ("simplemail", simpleMailTests)
#endif
      ]

testsToRun :: Connection -> [String] -> [Either String Test]
testsToRun _ [] = []
testsToRun conn (t:ts) 
  | lt == "all" = map (Right . snd) (allTests conn) ++ rest
  | otherwise = case lookup lt (allTests conn) of
                  Just testcase -> Right testcase : rest
                  Nothing       -> Left t : rest
  where
    lt = map toLower t
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
