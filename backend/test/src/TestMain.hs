module TestMain where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Base
import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Internal.Connection
import Log
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.IO
import Test.Framework
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T

import AccountInfoTest
import AdministrationTest
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
import CSSGenerationTest
import CSVUtilTest
import DB
import DB.PostgreSQL
import DB.SQLFunction
import DB.SQLTrigger
import Doc.API.V1.CallsTest
import Doc.API.V1.ForwardsCompatibilityTest
import Doc.API.V1.JSONTest
import Doc.API.V2.APILogTest
import Doc.API.V2.CallsTest
import Doc.API.V2.JSONTest
import Doc.QRCodeTest
import DocControlTest
import DocStateTest
import DumpEvidenceTexts
import ESignatureTest
import EvidenceAttachmentsTest
import EvidenceLogTest
import FeatureFlagsTest
import FileTest
import FlashMessages
import GTWorkflowTest
import HtmlTest
import InputValidationTest
import JSONUtilTest
import KontraPrelude
import LangTest
import LocalizationTest
import Log.Configuration
import LoginTest
import LogTest
import MailModelTest
import MailsTest
import Monitor.APITest
import OAuth
import PadApplication.APITest
import Partner.APITest
import ReferenceScreenshotsTest
import SessionsTest
import SignupTest
import Templates (readGlobalTemplates)
import TestKontra
import ThirdPartyStats
import User.APITest
import UserHistoryTest
import UserStateTest
import qualified HostClock.Model as HC

allTests :: [TestEnvSt -> Test]
allTests = [
    accountInfoTests
  , administrationTests
  , apiV1CallsTests
  , apiV1JSONTests
  , apiV1ForwardsCompatibilityTests
  , apiV2CallLogTests
  , apiV2CallsTests
  , apiV2JSONTests
  , archiveTests
  , brandedDomainTests
  , chargeableTest
  , companyAccountsTests
  , companyBrandingTests
  , companyControlTests
  , companyStateTests
  , configTests
  , cssGenerationTests
  , csvUtilTests
  , docControlTests
  , docStateTests
  , dumpAllEvidenceTexts
  , eSignatureTests
  , evidenceAttachmentsTest
  , evidenceLogTests
  , fileTests
  , featureFlagsTest
  , flashMessagesTests
  , htmlTests
  , inputValidationTests
  , jsonUtilTests
  , langTests
  , localizationTest
  , logTests
  , loginTests
  , mailModelTests
  , mailsTests
  , monitorAPITests
  , oauthTest
  , padAplicationAPITests
  , partnerAPITests
  , qrCodeTests
  , sessionsTests
  , signupTests
  , thirdPartyStatsTests
  , userAPITests
  , userHistoryTests
  , userStateTests
  ]

stagingTests :: [TestEnvSt -> Test]
stagingTests = [
    screenshotTests,
    gtWorkflowTests
  ]

modifyTestEnv :: [String] -> ([String], TestEnvSt -> TestEnvSt)
modifyTestEnv [] = ([], id)
modifyTestEnv ("--staging-tests":r) = second (. (\te -> te{ teStagingTests = True})) $
                                   modifyTestEnv r
modifyTestEnv ("--output-dir":d:r) = second (. (\te -> te{ teOutputDirectory = Just d})) $
                                   modifyTestEnv r
modifyTestEnv (d:r) = first (d:) $ modifyTestEnv r


testMany :: ([String], [(TestEnvSt -> Test)]) -> IO ()
testMany (allargs, ts) = do
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  lr  <- mkLogRunner "test" def rng
  withLogger lr $ \runLogger -> testMany' (allargs, ts) runLogger rng

testMany' :: ([String], [(TestEnvSt -> Test)])
          -> (forall m r . LogT m r -> m r) -> CryptoRNGState -> IO ()
testMany' (allargs, ts) runLogger rng = do
  let (args, envf) = modifyTestEnv allargs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf    <- T.readFile "kontrakcja_test.conf"
  templates <- readGlobalTemplates

  let connSettings = pgConnSettings pgconf

  runLogger . runDBT (unConnectionSource . simpleSource $ connSettings []) def $ do
    migrateDatabase [] kontraExtensions kontraDomains kontraTables kontraMigrations
    defineFunctions kontraFunctions
    defineComposites kontraComposites
    defineTriggers kontraTriggers
    offsets <- dbQuery $ HC.GetNClockErrorEstimates 10
    when (not $ HC.enoughClockErrorOffsetSamples offsets) $ do
      _ <- dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
      _ <- dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
      return ()
    commit

  staticSource <- (\conn -> ConnectionSource $ ConnectionSourceM { withConnection = ($ conn) }) <$> connect (connSettings kontraComposites)
  cs <- poolSource (connSettings kontraComposites) 1 10 50

  active_tests <- atomically $ newTVar (True, 0)
  rejected_documents <- atomically $ newTVar 0
  let env = envf $ TestEnvSt {
        teConnSource        = cs
      , teStaticConnSource  = staticSource
      , teTransSettings     = def
      , teRNGState          = rng
      , teRunLogger         = runLogger
      , teGlobalTemplates   = templates
      , teActiveTests       = active_tests
      , teRejectedDocuments = rejected_documents
      , teOutputDirectory   = Nothing
      , teStagingTests      = False
      }
      ts' = if teStagingTests env
        then stagingTests ++ ts
        else ts
  case teOutputDirectory env of
    Nothing -> return ()
    Just d  -> createDirectoryIfMissing True d
  E.finally (defaultMainWithArgs (map ($ env) ts') args) $ do
    -- upon interruption (eg. Ctrl+C), prevent next tests in line
    -- from running and wait until all that are running are finished.
    atomically . modifyTVar' active_tests $ first (const False)
    atomically $ do
      n <- snd <$> readTVar active_tests
      when (n /= 0) retry
    runDBT (unConnectionSource staticSource) def { tsAutoTransaction = False } $ do
      stats <- getConnectionStats
      liftBase . putStrLn $ "SQL: " ++ show stats
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
main = do
  args <- getArgs
  testMany (args, allTests)
