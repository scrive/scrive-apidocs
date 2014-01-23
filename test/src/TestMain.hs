{-# LANGUAGE CPP #-}
module TestMain where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import System.Directory (createDirectoryIfMissing)
import System.Environment.UTF8
import System.IO
import Test.Framework
import qualified Log
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import AppDBTables
import AppDBMigrations
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
import DumpEvidenceTexts
import EvidenceAttachmentsTest
import HtmlTest
import LocalizationTest
import InputValidationTest
import LoginTest
import SignupTest
import AccountInfoTest
import MailModelTest
import UserStateTest
import UserHistoryTest
import CSVUtilTest
import LangTest
import CompanyAccountsTest
import MailsTest
import JSONUtilTest
import SQLUtilsTest
import SessionsTest
import FileTest
import EvidenceLogTest
import PadTest
--import LiveDocxTest
import OAuth
import FlashMessages
import PaymentsTest
import ThirdPartyStats

allTests :: [(String, TestEnvSt -> Test)]
allTests = tail tests
  where
    tests = [
        undefined
      , ("companystate",  companyStateTests)
      , ("companycontrol",  companyControlTests)
      , ("docstate",  docStateTests)
      , ("doccontrol",  docControlTests)
      , ("docapi",  docAPITests)
      , ("docstatequery", const $ docStateQueryTests)
      , ("evidenceattachments", const $ evidenceAttachmentsTest)
      , ("html", const $ htmlTests)
      , ("localization", const $ localizationTest)
      , ("inputvalidation", const $ inputValidationTests)
      , ("login", loginTests)
      , ("signup", signupTests)
      , ("accountinfo", accountInfoTests)
      , ("userstate", userStateTests)
      , ("userhistory", userHistoryTests)
      , ("csvutil", const $ csvUtilTests)
      , ("mailmodel", mailModelTests)
      , ("lang", langTests)
      , ("companyaccounts", companyAccountsTests)
      , ("mails", mailsTests )
      , ("jsonutil", jsonUtilTests )
      , ("file", fileTests )
      , ("sqlutil", sqlUtilsTests )
      , ("evidencelog", evidenceLogTests)
      , ("evidencetexts", dumpAllEvidenceTexts)
      , ("pad", padTests)
--      , ("livedocx", const $ const liveDocxTests)
      , ("oauth", oauthTest)
      , ("flashmessages", const $ flashMessagesTests)
      , ("payments", paymentsTests)
      , ("sessions", sessionsTests)
      , ("thirdpartystats", thirdPartyStatsTests)
      ]

testsToRun :: [String] -> [Either String (TestEnvSt -> Test)]
testsToRun [] = []
testsToRun (t:ts)
  | lt == "all" = map (\(_,f) -> Right $ f) allTests ++ rest
  | otherwise = case lookup lt allTests of
                  Just testcase -> Right (testcase) : rest
                  Nothing       -> Left t : rest
  where
    lt = map toLower t
    rest = testsToRun ts

modifyTestEnv :: [String] -> ([String], TestEnvSt -> TestEnvSt)
modifyTestEnv [] = ([], id)
modifyTestEnv ("--output-dir":d:r) = second (. (\te -> te{ teOutputDirectory = Just d})) $
                                   modifyTestEnv r
modifyTestEnv (d:r) = first (d:) $ modifyTestEnv r


testMany :: ([String], [(TestEnvSt -> Test)]) -> IO ()
testMany (allargs, ts) = Log.withLogger $ do
  let (args, envf) =  modifyTestEnv allargs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- readFile "kontrakcja_test.conf"
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  templates <- readGlobalTemplates
  withPostgreSQL pgconf $ do
    migrateDatabase Log.mixlog_ kontraTables kontraMigrations
    defineMany kontraFunctions

    nex <- getNexus
    active_tests <- liftIO . atomically $ newTVar (True, 0)
    rejected_documents <- liftIO . atomically $ newTVar 0
    let env = envf $ TestEnvSt {
          teNexus = nex
        , teRNGState = rng
        , teGlobalTemplates = templates
        , teActiveTests = active_tests
        , teRejectedDocuments = rejected_documents
        , teOutputDirectory = Nothing
        }
    case teOutputDirectory env of
      Nothing -> return ()
      Just d  -> liftIO $ createDirectoryIfMissing True d
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
