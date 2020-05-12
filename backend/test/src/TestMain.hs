module TestMain where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Monad.Base
import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Internal.Connection
import Log
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath ((</>))
import System.IO
import Test.Framework
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import qualified Data.Traversable as T

import AccessControl.APITest
import AccessControl.EIDTest
import AccessControl.RoleTest
import AccountInfoTest
import AdministrationTest
import AppDBMigrations
import AppDBTables
import AppDir (AppPaths(..), setupAppPaths)
import ArchiveTest
import Attachment.APITest
import BrandedDomainTest
import ChargeableTest
import CompanyBrandingTest
import CompanyControlTest
import ConfigTests
import Configuration
import CronMonthlyInvoiceTest
import CronStatsTest
import CSSGenerationTest
import CSVUtilTest
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import DB.SQLFunction
import DB.SQLTrigger
import Doc.AccessControlTest
import Doc.API.V1.CallsTest
import Doc.API.V1.ForwardsCompatibilityTest
import Doc.API.V1.JSONTest
import Doc.API.V2.APILogTest
import Doc.API.V2.CallsTest
import Doc.API.V2.JSONTest
import DocControlTest
import DocStateTest
import DumpEvidenceTexts
import EID.EIDService.CommunicationTest
import ESignatureTest
import EvidenceAttachmentsTest
import EvidenceLogTest
import FeatureFlagsTest
import FileStorage
import FileStorage.Amazon.S3Env
import FileTest
import FlashMessages
import Flow.Server
import FlowTests
import Folder.APITest
import Folder.FolderTest
import Generators.DocumentGeneratorsTest
import Generators.OccurenceControlTest
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
import PdfToolsLambda.Conf
import QRCodeTest
import ReferenceScreenshotsTest
import SessionsTest
import SignupTest
import SMSLinkShorteningTest
import SSO.GuardsTest
import SSO.SAMLTest
import Templates (readGlobalTemplates)
import TestConf
import TestKontra
import ThirdPartyStats
import User.APITest
import UserGroup.APITest
import UserGroup.UserGroupTest
import UserGroupAccountsTest
import UserHistoryTest
import UserPasswordTest
import UserStateTest
import qualified HostClock.Model as HC
import qualified TestEnvSt.Internal

allTests :: [TestEnvSt -> Test]
allTests =
  [ accessControlApiTests
  , accessControlEIDTests
  , accessControlRoleTests
  , accountInfoTests
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
  , cronStatsTest
  , cssGenerationTests
  , csvUtilTests
  , docControlTests
  , docStateSideEffectsTests
  , docStateTests
  , docAccessControlTests
  , dumpAllEvidenceTexts
  , eSignatureTests
  , evidenceAttachmentsTest
  , evidenceLogTests
  , folderApiTests
  , fileTests
  , featureFlagsTest
  , flashMessagesTests
  , flowTests
  , folderTests
  , userGroupTests
  , userGroupApiTests
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
  , nemidTests
  , oauthTest
  , padAplicationAPITests
  , partnerAPITests
  , qrCodeTests
  , smsLinkShorteningTest
  , sessionsTests
  , signupTests
  , thirdPartyStatsTests
  , userAPITests
  , userHistoryTests
  , userPasswordTests
  , userStateTests
  , documentGeneratorsTests
  , occurenceControlTests
  , samlConditionsGuardsTests
  , samlSignatureTest
  ]

stagingTests :: [TestEnvSt -> Test]
stagingTests = [screenshotTests, gtWorkflowTests]

modifyTestEnv :: [String] -> ([String], TestEnvSt -> TestEnvSt)
modifyTestEnv [] = ([], identity)
modifyTestEnv ("--staging-tests" : r) =
  second (. set #stagingTests True) $ modifyTestEnv r
modifyTestEnv ("--output-dir" : d : r) =
  second (. set #outputDirectory (Just d)) $ modifyTestEnv r
modifyTestEnv (d : r) = first (d :) $ modifyTestEnv r


testMany :: FilePath -> ([String], [TestEnvSt -> Test]) -> IO ()
testMany workspaceRoot (allargs, ts) = do
  rng        <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  (errs, lr) <- mkLogRunner "test" testLogConfig rng
  mapM_ T.putStrLn errs

  tconf@TestConf {..} <- readConfig putStrLn (workspaceRoot </> "kontrakcja_test.conf")

  void . fork $ runFlow
    lr
    (FlowConfiguration
      (unConnectionSource . simpleSource $ pgConnSettings testDBConfig [])
      testFlowPort
    )

  withLogger lr $ \runLogger -> testMany' tconf (allargs, ts) runLogger rng

testMany'
  :: TestConf
  -> ([String], [TestEnvSt -> Test])
  -> (forall m r . LogT m r -> m r)
  -> CryptoRNGState
  -> IO ()
testMany' tconf (allargs, ts) runLogger rng = do
  let (args, envf) = modifyTestEnv allargs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  templates <- readGlobalTemplates

  let connSettings  = pgConnSettings (testDBConfig tconf)
      extrasOptions = defaultExtrasOptions
  runLogger
    . runDBT (unConnectionSource . simpleSource $ connSettings [])
             defaultTransactionSettings
    $ do
        migrateDatabase extrasOptions
                        kontraExtensions
                        kontraComposites
                        kontraDomains
                        kontraTables
                        kontraMigrations
        defineFunctions kontraFunctions
        defineTriggers kontraTriggers
        offsets <- dbQuery $ HC.GetNClockErrorEstimates 10
        unless (HC.enoughClockErrorOffsetSamples offsets) $ do
          void . dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
          void . dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
        commit

  staticSource <-
    (\conn -> ConnectionSource $ ConnectionSourceM { withConnection = ($ conn) })
      <$> connect (connSettings kontraComposites)
  cs                 <- poolSource (connSettings kontraComposites) 1 10 50

  active_tests       <- atomically $ newTVar (True, 0)
  rejected_documents <- newMVar 0
  test_durations     <- newMVar []
  memcache <- newFileMemCache . fromMaybe 200000000 $ testLocalFileCacheSize tconf
  mRedisConn         <- T.forM (testRedisCacheConfig tconf) mkRedisConnection
  mAmazonEnv         <- sequence (s3envFromConfig <$> testAmazonConfig tconf)
  lambdaEnv          <- pdfToolsLambdaEnvFromConf $ testPdfToolsLambdaConf tconf
  let env = envf $ TestEnvSt { connSource         = cs
                             , staticConnSource   = staticSource
                             , transSettings      = defaultTransactionSettings
                             , rngState           = rng
                             , runLogger          = RunLogger runLogger
                             , globalTemplates    = templates
                             , activeTests        = active_tests
                             , rejectedDocuments  = rejected_documents
                             , outputDirectory    = Nothing
                             , stagingTests       = False
                             , pdfToolsLambdaEnv  = lambdaEnv
                             , amazonS3Env        = mAmazonEnv
                             , fileMemCache       = memcache
                             , redisConn          = mRedisConn
                             , cronDBConfig       = testDBConfig tconf
                             , cronMonthlyInvoice = testMonthlyInvoiceConf tconf
                             , testDurations      = test_durations
                             , flowPort           = testFlowPort tconf
                             }
      ts' = if env ^. #stagingTests then stagingTests ++ ts else ts
  forM_ (env ^. #outputDirectory) $ createDirectoryIfMissing True
  E.finally (defaultMainWithArgs (map ($ env) ts') args) $ do
    -- Upon interruption (eg. Ctrl+C), prevent next tests in line
    -- from running and wait until all that are running are finished.
    atomically . modifyTVar' active_tests $ first (const False)
    atomically $ do
      n <- snd <$> readTVar active_tests
      when (n /= 0) retry
    runDBT (unConnectionSource staticSource)
           defaultTransactionSettings { tsAutoTransaction = False }
      $ do
          stats <- getConnectionStats
          liftBase . putStrLn $ "SQL: " ++ show stats
    rejs <- readMVar rejected_documents
    putStrLn $ "Documents generated but rejected: " ++ show rejs
    let testLongDuration = 5
    tds <- sortOn fst . filter ((> testLongDuration) . fst) <$> readMVar test_durations
    putStrLn
      $  "Tests that took longer than "
      ++ show testLongDuration
      ++ " seconds to run:"
    forM_ tds $ \(diff, s) -> putStrLn $ show diff ++ ": " ++ s

-- | Useful for running an individual test in ghci like so:
--
-- > testone flip (testThat "")
-- > testPreparationAttachCSVUploadNonExistingSignatoryLink
testone :: (TestEnvSt -> Test) -> IO ()
testone t = do
  args <- getArgs
  (AppPaths _ workspaceRoot) <- setupAppPaths
  testMany workspaceRoot (args, [t])

main :: IO ()
main = do
  args <- getArgs
  (AppPaths _ workspaceRoot) <- setupAppPaths
  testMany workspaceRoot (args, allTests)
