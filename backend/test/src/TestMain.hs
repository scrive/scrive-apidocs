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
import qualified Data.Traversable as T

import AccountInfoTest
import AdministrationTest
import AppDBMigrations
import AppDBTables
import ArchiveTest
import Attachment.APITest
import BrandedDomainTest
import ChargeableTest
import CompanyBrandingTest
import CompanyControlTest
import ConfigTests
import Configuration
import CronMonthlyInvoiceTest
import CSSGenerationTest
import CSVUtilTest
import Database.Redis.Configuration
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
import DocControlTest
import DocStateTest
import DumpEvidenceTexts
import ESignatureTest
import EvidenceAttachmentsTest
import EvidenceLogTest
import FeatureFlagsTest
import FileStorage
import FileTest
import FlashMessages
import GTWorkflowTest
import HtmlTest
import InputValidationTest
import JSONUtilTest
import LangTest
import LocalizationTest
import Log.Configuration
import LoginTest
import LogTest
import MailModelTest
import MailsTest
import Monitor.APITest
import NetsXMLTest
import OAuth
import PadApplication.APITest
import Partner.APITest
import QRCodeTest
import ReferenceScreenshotsTest
import SessionsTest
import SignupTest
import Templates (readGlobalTemplates)
import TestConf
import TestEnvSt.Internal
import TestKontra
import ThirdPartyStats
import User.APITest
import UserGroup.UserGroupTest
import UserGroupAccountsTest
import UserHistoryTest
import UserPasswordTest
import UserStateTest
import qualified HostClock.Model as HC

allTests :: [TestEnvSt -> Test]
allTests = [
    accountInfoTests
  , administrationTests
  , apiV1CallsTests
  , apiV1ForwardsCompatibilityTests
  , apiV1JSONTests
  , apiV2CallLogTests
  , apiV2CallsTests
  , apiV2JSONTests
  , archiveTests
  , attachmentAPITests
  , brandedDomainTests
  , chargeableTest
  , companyAccountsTests
  , companyBrandingTests
  , companyControlTests
  , configTests
  , cronMonthlyInvoiceTest
  , cssGenerationTests
  , csvUtilTests
  , docControlTests
  , docStateSideEffectsTests
  , docStateTests
  , dumpAllEvidenceTexts
  , eSignatureTests
  , evidenceAttachmentsTest
  , evidenceLogTests
  , fileTests
  , featureFlagsTest
  , flashMessagesTests
  , userGroupTests
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
  , netsXmlTests
  , oauthTest
  , padAplicationAPITests
  , partnerAPITests
  , qrCodeTests
  , sessionsTests
  , signupTests
  , thirdPartyStatsTests
  , userAPITests
  , userHistoryTests
  , userPasswordTests
  , userStateTests
  ]

stagingTests :: [TestEnvSt -> Test]
stagingTests = [
    screenshotTests,
    gtWorkflowTests
  ]

modifyTestEnv :: [String] -> ([String], TestEnvSt -> TestEnvSt)
modifyTestEnv [] = ([], id)
modifyTestEnv ("--staging-tests":r) =
  second (. set teStagingTests True) $ modifyTestEnv r
modifyTestEnv ("--output-dir":d:r) =
  second (. set teOutputDirectory (Just d)) $ modifyTestEnv r
modifyTestEnv (d:r) = first (d:) $ modifyTestEnv r


testMany :: ([String], [(TestEnvSt -> Test)]) -> IO ()
testMany (allargs, ts) = do
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  (errs, lr)  <- mkLogRunner "test" testLogConfig rng
  mapM_ T.putStrLn errs

  withLogger lr $ \runLogger -> testMany' (allargs, ts) runLogger rng

testMany' :: ([String], [(TestEnvSt -> Test)])
          -> (forall m r . LogT m r -> m r) -> CryptoRNGState -> IO ()
testMany' (allargs, ts) runLogger rng = do
  let (args, envf) = modifyTestEnv allargs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  tconf     <- readConfig  putStrLn "kontrakcja_test.conf"
  templates <- readGlobalTemplates

  let connSettings = pgConnSettings (testDBConfig tconf)
      extrasOptions = def
  runLogger . runDBT (unConnectionSource . simpleSource $ connSettings []) def $ do
    migrateDatabase extrasOptions kontraExtensions
      kontraDomains kontraTables kontraMigrations
    defineFunctions kontraFunctions
    defineComposites kontraComposites
    defineTriggers kontraTriggers
    offsets <- dbQuery $ HC.GetNClockErrorEstimates 10
    unless (HC.enoughClockErrorOffsetSamples offsets) $ do
      void $ dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
      void $ dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
      return ()
    commit

  staticSource <-
    (\conn -> ConnectionSource $ ConnectionSourceM { withConnection = ($ conn) })
    <$> connect (connSettings kontraComposites)
  cs <- poolSource (connSettings kontraComposites) 1 10 50

  active_tests       <- atomically $ newTVar (True, 0)
  rejected_documents <- atomically $ newTVar 0
  memcache           <- newFileMemCache $
                        fromMaybe 200000000 $ testLocalFileCacheSize tconf
  mRedisConn         <- T.forM (testRedisCacheConfig tconf) mkRedisConnection
  let env = envf $ TestEnvSt {
        _teConnSource         = cs
      , _teStaticConnSource   = staticSource
      , _teTransSettings      = def
      , _teRNGState           = rng
      , _teRunLogger          = RunLogger runLogger
      , _teGlobalTemplates    = templates
      , _teActiveTests        = active_tests
      , _teRejectedDocuments  = rejected_documents
      , _teOutputDirectory    = Nothing
      , _teStagingTests       = False
      , _tePdfToolsLambdaConf = testPdfToolsLambdaConf tconf
      , _teAmazonConfig       = testAmazonConfig tconf
      , _teFileMemCache       = memcache
      , _teRedisConn          = mRedisConn
      , _teCronDBConfig       = testDBConfig tconf
      , _teCronMonthlyInvoice = testCronMonthlyInvoiceConf tconf
      }
      ts' = if get teStagingTests env
            then stagingTests ++ ts
            else ts
  case (get teOutputDirectory env) of
    Nothing -> return ()
    Just d  -> createDirectoryIfMissing True d
  E.finally (defaultMainWithArgs (map ($ env) ts') args) $ do
    -- Upon interruption (eg. Ctrl+C), prevent next tests in line
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
-- > testone flip (testThat "")
-- > testPreparationAttachCSVUploadNonExistingSignatoryLink
testone :: (TestEnvSt -> Test) -> IO ()
testone t = do
  args <- getArgs
  testMany (args, [t])

main :: IO ()
main = do
  args <- getArgs
  testMany (args, allTests)
