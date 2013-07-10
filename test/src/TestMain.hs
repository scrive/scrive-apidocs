{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import System.Environment.UTF8
import System.IO
import Test.Framework
import qualified Log
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import AppDB
import Crypto.RNG
import DB
import DB.Checks
import DB.SQLFunction
import DB.PostgreSQL
import Templates (readGlobalTemplates)
import TestKontra

-- Note: if you add new testsuites here, please add them in a similar
-- manner to existing ones, i.e. wrap them around ifdefs and add appropriate
-- flags to kontrakcja.cabal to allow possibility of disabling tests selectively
-- if e.g. for some reason they stop compiling. Also, please keep them in
-- alphabetic order.

import CompanyStateTest
import CompanyControlTest
import DocStateTest
import DocControlTest
import DocAPITest
import DocStateQueryTest
import EvidenceAttachmentsTest
import HtmlTest
import InputValidationTest
import LoginTest
import SignupTest
import AccountInfoTest
import MailAPITest
import MailModelTest
import RedirectTest
import UserStateTest
import UserHistoryTest
import CSVUtilTest
import SimpleMailTest
import LangTest
import CompanyAccountsTest
import MailsTest
import JSONUtilTest
import SQLUtilsTest
import SessionsTest
import FileTest
import Doc.TestJSON
import EvidenceLogTest
import PadTest
--import LiveDocxTest
import OAuth
import FlashMessages
import PaymentsTest
import ThirdPartyStats

allTests :: [(String, [String] -> TestEnvSt -> Test)]
allTests = tail tests
  where
    tests = [
        undefined
      , ("companystate", const companyStateTests)
      , ("companycontrol", const companyControlTests)
      , ("docstate", const docStateTests)
      , ("doccontrol", const docControlTests)
      , ("docapi", const docAPITests)
      , ("docstatequery", const $ const docStateQueryTests)
      , ("evidenceattachments", const $ const evidenceAttachmentsTest)
      , ("html", const $ const htmlTests)
      , ("inputvalidation", const $ const inputValidationTests)
      , ("login", const loginTests)
      , ("signup", const signupTests)
      , ("accountinfo", const accountInfoTests)
      , ("mailapi", const mailApiTests)
      , ("redirect", const redirectTests)
      , ("userstate", const userStateTests)
      , ("userhistory", const userHistoryTests)
      , ("csvutil", const $ const csvUtilTests)
      , ("simplemail", const $ const simpleMailTests)
      , ("mailmodel", const mailModelTests)
      , ("lang", const langTests)
      , ("companyaccounts", const companyAccountsTests)
      , ("mails", mailsTests )
      , ("jsonutil", const jsonUtilTests )
      , ("file", const fileTests )
      , ("docjson", const documentJSONTests)
      , ("sqlutil", const sqlUtilsTests )
      , ("evidencelog", const evidenceLogTests)
      , ("pad", const padTests)
--      , ("livedocx", const $ const liveDocxTests)
      , ("oauth", const oauthTest)
      , ("flashmessages", const $ const flashMessagesTests)
      , ("payments", const paymentsTests)
      , ("sessions", const sessionsTests)
      , ("thirdpartystats", const thirdPartyStatsTests)
      ]

testsToRun :: [String] -> [Either String (TestEnvSt -> Test)]
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

testMany :: ([String], [(TestEnvSt -> Test)]) -> IO ()
testMany (args, ts) = Log.withLogger $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- readFile "kontrakcja_test.conf"
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  templates <- readGlobalTemplates
  withPostgreSQL pgconf $ do
    performDBChecks Log.debug kontraTables kontraMigrations
    defineMany kontraFunctions
    nex <- getNexus
    active_tests <- liftIO . atomically $ newTVar (True, 0)
    rejected_documents <- liftIO . atomically $ newTVar 0
    let env = TestEnvSt {
          teNexus = nex
        , teRNGState = rng
        , teGlobalTemplates = templates
        , teActiveTests = active_tests
        , teRejectedDocuments = rejected_documents
        }
    liftIO . E.finally (defaultMainWithArgs (map ($ env) ts) args) $ do
      -- upon interruption (eg. Ctrl+C), prevent next tests in line
      -- from running and wait until all that are running are finished.
      atomically . modifyTVar' active_tests $ first (const False)
      atomically $ do
        n <- snd <$> readTVar active_tests
        when (n /= 0) retry
      stats <- getNexusStats nex
      putStrLn $ "SQL: " ++ show stats
      rejs <- atomically (readTVar rejected_documents)
      putStrLn $ "Documents generated but rejected: " ++ show rejs

-- | Useful for running an individual test in ghci like so:
--
-- >  testone flip (testThat "") testPreparationAttachCSVUploadNonExistingSignatoryLink
testone :: (TestEnvSt -> Test) -> IO ()
testone t = do
  args <- getArgs
  testMany (args, [t])

main :: IO ()
main = (partitionEithers . testsToRun <$> getArgs) >>= testMany
