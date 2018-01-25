{-# LANGUAGE PackageImports #-}
module Cron (main) where

import Control.Monad
import Control.Monad.Base
import Crypto.RNG
import Data.Maybe
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Log
import Network.HTTP.Client.TLS (newTlsManager)
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.Traversable as F

import Amazon.Consumer
import AppDBTables
import Configuration
import Cron.Model
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import Doc.API.Callback.Model
import Doc.Extending.Consumer
import Doc.Sealing.Consumer
import Doc.Signing.Consumer
import KontraError
import KontraPrelude hiding (All)
import Log.Configuration
import Monitoring
import Templates
import ThirdPartyStats.Mixpanel
import ThirdPartyStats.Planhat
import Utils.IO
import "kontrakcja" CronConf
import qualified MemCache
import qualified "kontrakcja" CronEnv

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

type CronM = CryptoRNGT (LogT IO)

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  cronConf <- readConfig putStrLn config
  case cronMonitoringConf cronConf of
    Just conf -> void $ startMonitoringServer conf
    Nothing   -> return ()
  rng <- newCryptoRNGState

  logRunner <- mkLogRunner "cron" (cronLogConfig cronConf) rng
  reqManager <- newTlsManager

  runWithLogRunner logRunner $ do
    checkExecutables

    let connSettings = pgConnSettings $ cronDBConfig cronConf
    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $
      checkDatabase kontraDomains kontraTables

    ConnectionSource pool <- ($ (maxConnectionTracker $ cronMaxDBConnections cronConf))
      <$> liftBase (createPoolSource (connSettings kontraComposites) (cronMaxDBConnections cronConf))
    templates <- liftBase readGlobalTemplates
    mrediscache <- F.forM (cronRedisCacheConfig cronConf) mkRedisConnection
    filecache <- MemCache.new BS.length (cronLocalFileCacheSize cronConf)

    -- Asynchronous event dispatcher; if you want to add a consumer to the event
    -- dispatcher, please combine the two into one dispatcher function rather
    -- than creating a new thread or something like that, since
    -- asyncProcessEvents removes events after processing.
    mmixpanel <- case cronMixpanelToken cronConf of
      Nothing -> do
        noConfigurationWarning "Mixpanel"
        return Nothing
      Just mt ->
        return $ Just $ processMixpanelEvent mt

    mplanhat <- case cronPlanhatConf cronConf of
      Nothing -> do
        noConfigurationWarning "Planhat"
        return Nothing
      Just phConf ->
        return . Just $ processPlanhatEvent reqManager phConf

    let runDB :: DBT CronM r -> CronM r
        runDB = withPostgreSQL pool

        runCronEnv :: CronEnv.CronEnvM r -> CronM r
        runCronEnv = runDB . CronEnv.runCronEnv cronConf
          filecache mrediscache templates

        docSealing   = documentSealing (cronAmazonConfig cronConf)
          (cronGuardTimeConf cronConf) templates filecache mrediscache pool (cronMailNoreplyAddress cronConf)
          (cronConsumerSealingMaxJobs cronConf)
        docSigning   = documentSigning (cronAmazonConfig cronConf)
          (cronGuardTimeConf cronConf) (cronCgiGrpConfig cronConf) (cronNetsSignConfig cronConf)
          templates filecache mrediscache pool (cronMailNoreplyAddress cronConf) (cronConsumerSigningMaxJobs cronConf)
        docExtending = documentExtendingConsumer (cronAmazonConfig cronConf)
          (cronGuardTimeConf cronConf) templates filecache mrediscache pool (cronConsumerExtendingMaxJobs cronConf)
        amazonFileUpload = amazonUploadConsumer (cronAmazonConfig cronConf) pool (cronConsumerAmazonMaxJobs cronConf)

        apiCallbacks = documentAPICallback runCronEnv (cronConsumerAPICallbackMaxJobs cronConf)
        cron = cronConsumer cronConf reqManager mmixpanel mplanhat runCronEnv runDB (cronConsumerCronMaxJobs cronConf)

    runCryptoRNGT rng
      . finalize (localDomain "document sealing"   $ runConsumer docSealing       pool)
      . finalize (localDomain "document signing"   $ runConsumer docSigning       pool)
      . finalize (localDomain "document extending" $ runConsumer docExtending     pool)
      . finalize (localDomain "api callbacks"      $ runConsumer apiCallbacks     pool)
      . finalize (localDomain "amazon file upload" $ runConsumer amazonFileUpload pool)
      . finalize (localDomain "cron"               $ runConsumer cron             pool)
      $ liftBase waitForTermination
