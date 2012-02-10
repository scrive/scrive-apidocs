{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Data.Char
import Data.Either
import System.Environment.UTF8
import System.IO
import Test.Framework
import qualified Log
import qualified Data.ByteString as BS

import Crypto.RNG (unsafeCryptoRNGState)
import AppDB
import DB.Checks
import DB.Classes
import DB.Nexus
import Control.Exception

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
import LocaleTest
#endif
#ifndef NO_COMPANYACCOUNTS
import CompanyAccountsTest
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
#ifndef NO_SQLUTILS
import SQLUtilsTest
#endif

#ifndef NO_FILE
import FileTest
#endif

#ifndef NO_DOCJSON
import Doc.TestJSON
#endif

#ifndef NO_STATS
import StatsTest
#endif

allTests :: [(String, [String] -> DBEnv -> Test)]
allTests = tail tests
  where
    tests = [
        undefined
#ifndef NO_COMPANYSTATE
      , ("companystate", const $ companyStateTests)
#endif
#ifndef NO_DOCSTATE
      , ("docstate", const $ docStateTests)
#endif
#ifndef NO_DOCCONTROL
      , ("doccontrol", const $ docControlTests)
#endif
#ifndef NO_DOCSTATEQUERY
      , ("docstatequery", const $ const docStateQueryTests)
#endif
#ifndef NO_HTML
      , ("html", const $ const htmlTests)
#endif
#ifndef NO_INPUTVALIDATION
      , ("inputvalidation", const $ const inputValidationTests)
#endif
#ifndef NO_INTEGRATIONAPI
      , ("integrationapi", const $ integrationAPITests)
#endif
#ifndef NO_LOGIN
      , ("login", const $ loginTests)
#endif
#ifndef NO_SIGNUP
      , ("signup", const $ signupTests)
#endif
#ifndef NO_ACCOUNTINFO
     , ("accountinfo", const $ accountInfoTests)
#endif
#ifndef NO_MAILAPI
      , ("mailapi", const $ mailApiTests)
#endif
#ifndef NO_REDIRECT
      , ("redirect", const $ const redirectTests)
#endif
#ifndef NO_SERVICESTATE
      , ("servicestate", const $ serviceStateTests)
#endif
#ifndef NO_TRUSTWEAVER
      -- everything fails for trustweaver, so commenting out for now
      , ("trustweaver", const $ const trustWeaverTests)
#endif
#ifndef NO_USERSTATE
      , ("userstate", const $ userStateTests)
#endif
#ifndef NO_CSVUTIL
      , ("csvutil", const $ const csvUtilTests)
#endif
#ifndef NO_SIMPLEEMAIL
      , ("simplemail", const $ const simpleMailTests)
#endif
#ifndef NO_LOCALE
      , ("locale", const $ localeTests)
#endif
#ifndef NO_COMPANYACCOUNTS
      , ("companyaccounts", const $ companyAccountsTests)
#endif
#ifndef NO_MAILS
      , ("mails", mailsTests )
#endif
#ifndef NO_MAILS
      , ("apicommons", const $ const apiCommonsTest )
#endif
#ifndef NO_JSON
      , ("jsonutil", const $ const jsonUtilTests )
#endif
#ifndef NO_FILE
      , ("file", const $ fileTests )
#endif
#ifndef NO_DOCJSON
      , ("docjson", const $ const documentJSONTests)
#endif
#ifndef NO_SQLUTILS
      , ("sqlutil", const $ const sqlUtilsTests )
#endif
#ifndef NO_STATS
      , ("stats", const $ statsTests)
#endif
      ]

testsToRun :: [String] -> [Either String (DBEnv -> Test)]
testsToRun [] = []
testsToRun (t:ts)
  | lt == "$" = []
  | lt == "all" = map (\(_,f) -> Right $ f params) allTests ++ rest
  | otherwise = case lookup lt allTests of
                  Just testcase -> Right (testcase params) : rest
                  Nothing       -> Left t : rest
  where
    lt = map toLower t
    rest = testsToRun ts
    params = drop 1 $ dropWhile (/= ("$")) ts

testMany :: ([String],[(DBEnv -> Test)]) -> IO ()
testMany (args, ts) = Log.withLogger $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- readFile "kontrakcja_test.conf"
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  withPostgreSQLDB' pgconf rng $ \dbenv -> do
    ioRunDB dbenv $ performDBChecks Log.debug kontraTables kontraMigrations

    -- defaultMainWithArgs does not feel like returning like a normal function
    -- so have to get around that 'feature'!!
    bracket_ (return ())
             (do
               stats <- getNexusStats (nexus dbenv)
               putStrLn $ "SQL: queries " ++ show (nexusQueries stats) ++ 
                          ", params " ++ show (nexusParams stats) ++
                          ", rows " ++ show (nexusRows stats) ++
                          ", values " ++ show (nexusValues stats))

             (defaultMainWithArgs (map ($ dbenv) ts) args)

-- | Useful for running an individual test in ghci like so:
--   @
--    testone flip (testThat "") testPreparationAttachCSVUploadNonExistingSignatoryLink
--   @
testone :: (DBEnv -> Test) -> IO ()
testone t = do
  args <- getArgs
  testMany (args, [t])

main :: IO ()
main = (partitionEithers . testsToRun <$> getArgs) >>= testMany

