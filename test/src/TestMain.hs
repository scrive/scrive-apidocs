module TestMain where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.PQTypes.Internal.Connection
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
import Templates (readGlobalTemplates)
import TestKontra

-- Note: if you add new testsuites here, please add them in a similar
-- manner to existing ones, i.e. wrap them around ifdefs and add appropriate
-- flags to kontrakcja.cabal to allow possibility of disabling tests selectively
-- if e.g. for some reason they stop compiling. Also, please keep them in
-- alphabetic order.

import AccountInfoTest
import BrandedDomainTest
import CSVUtilTest
import CompanyAccountsTest
import CompanyControlTest
import CompanyStateTest
import DocAPITest
import DocControlTest
import DocStateTest
import DumpEvidenceTexts
import EvidenceAttachmentsTest
import EvidenceLogTest
import FileTest
import FlashMessages
import HtmlTest
import InputValidationTest
import JSONUtilTest
import LangTest
import LocalizationTest
import LoginTest
import MailModelTest
import MailsTest
import OAuth
import PadTest
import PaymentsTest
import SessionsTest
import SignupTest
import ThirdPartyStats
import UserHistoryTest
import UserStateTest

allTests :: [TestEnvSt -> Test]
allTests = tail tests
  where
    tests = [
        undefined
      , accountInfoTests
      , brandedDomainTests
      , companyAccountsTests
      , companyControlTests
      , companyStateTests
      , csvUtilTests
      , docAPITests
      , docControlTests
      , docStateTests
      , evidenceAttachmentsTest
      , evidenceLogTests
      , dumpAllEvidenceTexts
      , fileTests
      , flashMessagesTests
      , htmlTests
      , inputValidationTests
      , jsonUtilTests
      , langTests
      , localizationTest
      , loginTests
      , mailModelTests
      , mailsTests
      , oauthTest
      , padTests
      , paymentsTests
      , sessionsTests
      , signupTests
      , thirdPartyStatsTests
      , userHistoryTests
      , userStateTests
      ]

modifyTestEnv :: [String] -> ([String], TestEnvSt -> TestEnvSt)
modifyTestEnv [] = ([], id)
modifyTestEnv ("--output-dir":d:r) = second (. (\te -> te{ teOutputDirectory = Just d})) $
                                   modifyTestEnv r
modifyTestEnv (d:r) = first (d:) $ modifyTestEnv r


testMany :: (TestEnvSt -> TestEnvSt) -> ([String], [(TestEnvSt -> Test)]) -> IO ()
testMany envf (args, ts) = Log.withLogger $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- BS.readFile "kontrakcja_test.conf"
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  templates <- readGlobalTemplates
  let connSettings = defaultSettings { csConnInfo = pgconf }
  conn <- connect connSettings
  let staticSource = ConnectionSource { withConnection = ($ conn) }
      connSource = defaultSource connSettings
      dts = defaultTransactionSettings
  runDBT connSource dts $ do
    migrateDatabase Log.mixlog_ kontraTables kontraMigrations
    defineMany kontraFunctions
    commit

    active_tests <- liftIO . atomically $ newTVar (True, 0)
    rejected_documents <- liftIO . atomically $ newTVar 0
    let env = envf $ TestEnvSt {
          teConnSource = connSource
        , teStaticConnSource = staticSource
        , teTransSettings = dts
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
      runDBT staticSource dts { tsAutoTransaction = False } $ do
        stats <- getConnectionStats
        liftIO . putStrLn $ "SQL: " ++ show stats
      rejs <- atomically (readTVar rejected_documents)
      putStrLn $ "Documents generated but rejected: " ++ show rejs

-- | Useful for running an individual test in ghci like so:
--
-- >  testone flip (testThat "") testPreparationAttachCSVUploadNonExistingSignatoryLink
testone :: (TestEnvSt -> Test) -> IO ()
testone t = do
  args <- getArgs
  testMany id (args, [t])

main :: IO ()
main = do
  allargs <- getArgs
  let (args, envf) = modifyTestEnv allargs
  testMany envf (partitionEithers $ testsToRun args)
