module TestMain where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Base
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Internal.Connection
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.IO
import Test.Framework
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T

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
import DB.PostgreSQL
import DB.SQLFunction
import Doc.API.V1.CallsTest
import Doc.API.V1.JSONTest
import Doc.API.V2.CallsTest
import Doc.API.V2.JSONTest
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
import KontraPrelude
import LangTest
import LocalizationTest
import Log.Configuration
import LoginTest
import MailModelTest
import MailsTest
import OAuth
import Partner.APITest
import PaymentsTest
import ReferenceScreenshotsTest
import SessionsTest
import SignupTest
import Templates (readGlobalTemplates)
import TestKontra
import ThirdPartyStats
import UserHistoryTest
import UserStateTest
import qualified HostClock.Model as HC

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
  , apiV1JSONTests
  , apiV1CallsTests
  , apiV2CallsTests
  , apiV2JSONTests
  , partnerAPITests
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

stagingTests :: [TestEnvSt -> Test]
stagingTests = [
    screenshotTests
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
  let (args, envf) = modifyTestEnv allargs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  pgconf <- T.readFile "kontrakcja_test.conf"
  rng <- unsafeCryptoRNGState (BS.pack (replicate 128 0))
  templates <- readGlobalTemplates

  let connSettings = pgConnSettings pgconf
  lr@LogRunner{..} <- mkLogRunner "test" def rng
  withLogger . runDBT (unConnectionSource . simpleSource $ connSettings []) def $ do
    migrateDatabase [] kontraExtensions kontraDomains kontraTables kontraMigrations
    defineFunctions kontraFunctions
    defineComposites kontraComposites
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
        teConnSource = cs
      , teStaticConnSource = staticSource
      , teTransSettings = def
      , teRNGState = rng
      , teLogRunner = lr
      , teGlobalTemplates = templates
      , teActiveTests = active_tests
      , teRejectedDocuments = rejected_documents
      , teOutputDirectory = Nothing
      , teStagingTests = False
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
