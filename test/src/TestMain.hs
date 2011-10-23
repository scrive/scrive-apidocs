{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import Database.HDBC.PostgreSQL
import System.Environment.UTF8
import System.IO
import Test.Framework
import qualified AppLogger as Log

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
#ifndef NO_SIGNUP
import SignupTest
#endif
#ifndef NO_ACCOUNTINFO
import AccountInfoTest
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
#ifndef NO_LOCALE
import LocaleTest
#endif
#ifndef NO_MAILS
import MailsTest
#endif
#ifndef NO_MAILS
import APICommonsTest
#endif
#ifndef NO_JSON
import JSONUtilTest
#endif

allTests :: Connection -> [(String, [String] -> Test)]
allTests conn = tail tests
  where
    tests = [
        undefined
#ifndef NO_COMPANYSTATE
      , ("companystate", const $ companyStateTests conn)
#endif
#ifndef NO_DOCSTATE
      , ("docstate", const $ docStateTests conn)
#endif
#ifndef NO_DOCCONTROL
      , ("doccontrol", const $ docControlTests conn)
#endif
#ifndef NO_DOCSTATEQUERY
      , ("docstatequery", const $ docStateQueryTests)
#endif
#ifndef NO_HTML
      , ("html", const $ htmlTests)
#endif
#ifndef NO_INPUTVALIDATION
      , ("inputvalidation", const $ inputValidationTests)
#endif
#ifndef NO_INTEGRATIONAPI
      , ("integrationapi", const $ integrationAPITests conn)
#endif
#ifndef NO_LOGIN
      , ("login", const $ loginTests conn)
#endif
#ifndef NO_SIGNUP
      , ("signup", const $ signupTests conn)
#endif
#ifndef NO_ACCOUNTINFO
     , ("accountinfo", const $ accountInfoTests conn)
#endif
#ifndef NO_MAILAPI
      , ("mailapi", const $ mailApiTests conn)
#endif
#ifndef NO_REDIRECT
      , ("redirect", const $ redirectTests)
#endif
#ifndef NO_SERVICESTATE
      , ("servicestate", const $ serviceStateTests conn)
#endif
#ifndef NO_TRUSTWEAVER
      -- everything fails for trustweaver, so commenting out for now
      --("trustweaver", trustWeaverTest)
#endif
#ifndef NO_USERSTATE
      , ("userstate", const $ userStateTests conn)
#endif
#ifndef NO_CSVUTIL
      , ("csvutil", const $ csvUtilTests)
#endif
#ifndef NO_SIMPLEEMAIL
      , ("simplemail", const $ simpleMailTests)
#endif
#ifndef NO_LOCALE
      , ("locale", const $ localeTests conn)
#endif
#ifndef NO_MAILS
      , ("mails", mailsTests conn )
#endif
#ifndef NO_MAILS
      , ("apicommons", const $ apiCommonsTest )
#endif
#ifndef NO_JSON
      , ("jsonutil", const $ jsonUtilTests )
#endif
      ]

testsToRun :: Connection -> [String] -> [Either String Test]
testsToRun _ [] = []
testsToRun conn (t:ts)
  | lt == "$" = []
  | lt == "all" = map (\(_,f) -> Right $ f params) (allTests conn) ++ rest
  | otherwise = case lookup lt (allTests conn) of
                  Just testcase -> Right (testcase params) : rest
                  Nothing       -> Left t : rest
  where
    lt = map toLower t
    rest = testsToRun conn ts
    params = drop 1 $ dropWhile (/= ("$")) ts

main :: IO ()
main = Log.withLogger $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- readFile "kontrakcja_test.conf"
  withPostgreSQL pgconf $ \conn -> do
    ioRunDB conn checkDBConsistency
    (args, tests) <- partitionEithers . testsToRun conn <$> getArgs
    defaultMainWithArgs tests args
