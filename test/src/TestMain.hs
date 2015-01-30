module TestMain where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Database.PostgreSQL.PQTypes.Internal.Connection
import System.Directory (createDirectoryIfMissing)
import System.Environment.UTF8
import System.IO
import Test.Framework
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import AccountInfoTest
import AppDBMigrations
import AppDBTables
import ArchiveTest
import BrandedDomainTest
import ChargeableTest
import CompanyAccountsTest
import CompanyBrandingTest
import CompanyControlTest
import CompanyStateTest
import ConfigTests
import Crypto.RNG
import CSSGenerationTest
import CSVUtilTest
import DB
import DB.Checks
import DB.PostgreSQL
import DB.SQLFunction
import DocAPITest
import DocControlTest
import DocStateTest
import DumpEvidenceTexts
import ESignatureTest
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
import PaymentsTest
import SessionsTest
import SignupTest
import Templates (readGlobalTemplates)
import TestKontra
import ThirdPartyStats
import UserHistoryTest
import UserStateTest
import qualified Log

allTests :: [TestEnvSt -> Test]
allTests = [
    accountInfoTests
  , brandedDomainTests
  , chargeableTest
  , companyAccountsTests
  , companyBrandingTests
  , companyControlTests
  , companyStateTests
  , configTests
  , cssGenerationTests
  , csvUtilTests
  , docAPITests
  , archiveTests
  , docControlTests
  , docStateTests
  , eSignatureTests
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


testMany :: ([String], [(TestEnvSt -> Test)]) -> Log.LogT IO ()
testMany (allargs, ts) = do
  let (args, envf) = modifyTestEnv allargs
  liftBase $ hSetEncoding stdout utf8
  liftBase $ hSetEncoding stderr utf8
  pgconf <- liftBase $ BS.readFile "kontrakcja_test.conf"
  rng <- liftBase $ unsafeCryptoRNGState (BS.pack (replicate 128 0))
  templates <- liftBase $ readGlobalTemplates

  let connSettings = pgConnSettings pgconf
      dts = defaultTransactionSettings
  runDBT (defaultSource $ connSettings []) dts $ do
    migrateDatabase Log.mixlog_ kontraDomains kontraTables kontraMigrations
    defineFunctions kontraFunctions
    defineComposites kontraComposites
    commit

  staticSource <- (\conn -> ConnectionSource { withConnection = ($ conn) })
    <$> liftBase (connect $ connSettings kontraComposites)

  active_tests <- liftBase . atomically $ newTVar (True, 0)
  rejected_documents <- liftBase . atomically $ newTVar 0
  let env = envf $ TestEnvSt {
        teConnSource = defaultSource $ connSettings kontraComposites
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
    Just d  -> liftBase $ createDirectoryIfMissing True d
  liftBase . E.finally (defaultMainWithArgs (map ($ env) ts) args) $ do
    -- upon interruption (eg. Ctrl+C), prevent next tests in line
    -- from running and wait until all that are running are finished.
    atomically . modifyTVar' active_tests $ first (const False)
    atomically $ do
      n <- snd <$> readTVar active_tests
      when (n /= 0) retry
    runDBT staticSource dts { tsAutoTransaction = False } $ do
      stats <- getConnectionStats
      liftBase . putStrLn $ "SQL: " ++ show stats
    rejs <- atomically (readTVar rejected_documents)
    putStrLn $ "Documents generated but rejected: " ++ show rejs

-- | Useful for running an individual test in ghci like so:
--
-- >  testone flip (testThat "") testPreparationAttachCSVUploadNonExistingSignatoryLink
testone :: (TestEnvSt -> Test) -> Log.LogT IO ()
testone t = do
  args <- liftBase getArgs
  testMany (args, [t])

main :: IO ()
main = Log.withLogger $ do
  args <- liftBase getArgs
  testMany (args, allTests)
