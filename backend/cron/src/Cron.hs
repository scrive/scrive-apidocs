module Cron (main) where

import Control.Monad.Base
import Crypto.RNG
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Log
import Network.HTTP.Client.TLS (newTlsManager)
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Data.Text.IO as T
import qualified Data.Traversable as F

import AppDBTables
import Configuration
import Cron.Model
import CronConf
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import Doc.API.Callback.Model
import Doc.Extending.Consumer
import Doc.Sealing.Consumer
import Doc.Signing.Consumer
import FileStorage
import FileStorage.Amazon.S3Env
import KontraError
import Log.Configuration
import Monitoring
import PdfToolsLambda.Conf
import Purging.Files
import Templates
import ThirdPartyStats.Core
import ThirdPartyStats.Mixpanel
import ThirdPartyStats.Planhat
import Utils.IO
import qualified CronEnv

data CmdConf = CmdConf {
  config :: String
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE"
} &= program progName
  where
    configFile = "cron.conf"

----------------------------------------

type CronM = FileStorageT (CryptoRNGT (LogT IO))

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  cronConf <- readConfig putStrLn config
  case cronMonitoringConf cronConf of
    Just conf -> void $ startMonitoringServer conf
    Nothing   -> return ()
  rng <- newCryptoRNGState

  (errs, logRunner) <- mkLogRunner "cron" (cronLogConfig cronConf) rng
  mapM_ T.putStrLn errs

  reqManager <- newTlsManager

  runWithLogRunner logRunner $ do
    checkExecutables

    let connSettings  = pgConnSettings $ cronDBConfig cronConf
        extrasOptions = defaultExtrasOptions { eoEnforcePKs = True }
    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $
      checkDatabase extrasOptions kontraComposites kontraDomains kontraTables

    ConnectionSource pool <- ($ (maxConnectionTracker $ cronMaxDBConnections cronConf))
      <$> liftBase (createPoolSource (connSettings kontraComposites) (cronMaxDBConnections cronConf))
    templates <- liftBase readGlobalTemplates
    mrediscache <- F.forM (cronRedisCacheConfig cronConf) mkRedisConnection
    filecache <- newFileMemCache $ cronLocalFileCacheSize cronConf

    -- Asynchronous event dispatcher; if you want to add a consumer to the event
    -- dispatcher, please combine the two into one dispatcher function rather
    -- than creating a new thread or something like that, since
    -- asyncProcessEvents removes events after processing.
    mmixpanel <- case cronMixpanelToken cronConf of
      Nothing -> do
        noConfigurationWarning "Mixpanel"
        return Nothing
      Just mt ->
        return . Just . EventProcessor $ processMixpanelEvent mt

    mplanhat <- case cronPlanhatConf cronConf of
      Nothing -> do
        noConfigurationWarning "Planhat"
        return Nothing
      Just phConf ->
        return . Just . EventProcessor $ processPlanhatEvent reqManager phConf

    amazonEnv <- s3envFromConfig $ cronAmazonConfig cronConf
    lambdaEnv <- pdfToolsLambdaEnvFromConf $ cronPdfToolsLambdaConf cronConf

    let runDB :: DBT CronM r -> CronM r
        runDB = withPostgreSQL pool

        runCronEnv :: CronEnv.CronEnvM r -> CronM r
        runCronEnv = runDB . CronEnv.runCronEnv cronConf templates

        docSealing   = documentSealing
          (cronGuardTimeConf cronConf) lambdaEnv templates pool
          (cronMailNoreplyAddress cronConf) (cronConsumerSealingMaxJobs cronConf)
        docSigning   = documentSigning
          (cronGuardTimeConf cronConf) (cronCgiGrpConfig cronConf) (cronNetsSignConfig cronConf)
          templates pool (cronMailNoreplyAddress cronConf) (cronConsumerSigningMaxJobs cronConf)
        docExtending = documentExtendingConsumer
          (cronGuardTimeConf cronConf) templates pool (cronConsumerExtendingMaxJobs cronConf)
        apiCallbacks = documentAPICallback runCronEnv (cronConsumerAPICallbackMaxJobs cronConf)
        cron = cronConsumer cronConf reqManager mmixpanel mplanhat runCronEnv runDB (cronConsumerCronMaxJobs cronConf)
        filePurging = filePurgingConsumer pool (cronConsumerFilePurgingMaxJobs cronConf)

    runCryptoRNGT rng
      . runFileStorageT (amazonEnv, mrediscache, filecache)
      . finalize (localDomain "document sealing"   $ runConsumer docSealing       pool)
      . finalize (localDomain "document signing"   $ runConsumer docSigning       pool)
      . finalize (localDomain "document extending" $ runConsumer docExtending     pool)
      . finalize (localDomain "api callbacks"      $ runConsumer apiCallbacks     pool)
      . finalize (localDomain "cron"               $ runConsumer cron             pool)
      . finalize (localDomain "file purging"       $ runConsumer filePurging      pool)
      $ liftBase waitForTermination
