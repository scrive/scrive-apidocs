module TestCron (runTestCronUntilIdle) where

import Control.Concurrent.STM
import Control.Monad.Loops
import Control.Monad.Reader
import Database.PostgreSQL.Consumers
import Log
import Network.HTTP.Client.TLS (newTlsManager)

import Amazon (AmazonConfig(..), runAmazonMonadT)
import Amazon.Consumer
import Context
import Cron.Model
import CronConf
import DB
import Doc.API.Callback.Model
import Doc.Extending.Consumer
import Doc.Sealing.Consumer
import Doc.Signing.Consumer
import TestKontra
import qualified CronEnv

runTestCronUntilIdle :: Context -> TestEnv ()
runTestCronUntilIdle ctx = do
  -- This is intented to run as part of a test and tests are usually one big transaction.
  -- Running consumers spawns additional processes with their own DB transactions,
  -- which do not see changes of the test transaction ... unless there is a commit.
  commit
  ConnectionSource pool <- asks teConnSource
  -- Will not be used, because Planhat is not configured when testing, but it is a parameter for cronConsumer.
  reqManager <- newTlsManager

  let -- for testing, one of each is sufficient
      cronConf = CronConf {
          cronAmazonConfig       = Nothing
        , cronDBConfig           =
            "user='kontra' password='kontra' dbname='kontrakcja'"
        , cronMaxDBConnections   = 100
        , cronRedisCacheConfig   = Nothing
        , cronLocalFileCacheSize = 200000000
        , cronLogConfig          = testLogConfig
        , cronGuardTimeConf      = testGTConf
        , cronCgiGrpConfig       = Nothing
        , cronMixpanelToken      = Nothing
        , cronNtpServers         = [ show n ++ ".ubuntu.pool.ntp.org" | n <- [0..3] ]
        , cronSalesforceConf     = Nothing
        , cronInvoicingSFTPConf  = Nothing
        , cronPlanhatConf        = Nothing
        , cronMonitoringConf     = Nothing
        , cronMailNoreplyAddress = "noreply@scrive.com"
        , cronConsumerCronMaxJobs        = 1
        , cronConsumerSealingMaxJobs     = 1
        , cronConsumerSigningMaxJobs     = 1
        , cronConsumerExtendingMaxJobs   = 1
        , cronConsumerAPICallbackMaxJobs = 1
        , cronConsumerAmazonMaxJobs      = 1
        , cronNetsSignConfig = Nothing
        }

      -- make timeouts small, so that the test runs faster
      modTimeout = \c -> c { ccNotificationTimeout = 100 * 1000 }
      cronPartRunners =
        [ ( "document sealing"
          , runConsumerWithIdleSignal . modTimeout
            $ documentSealing (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                (get ctxglobaltemplates ctx) (get ctxfilecache ctx) (get ctxmrediscache ctx) pool
                (cronMailNoreplyAddress cronConf) (cronConsumerSealingMaxJobs cronConf)
          )
        , ( "document signing"
          , runConsumerWithIdleSignal . modTimeout
            $ documentSigning (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                (cronCgiGrpConfig cronConf) (cronNetsSignConfig cronConf) (get ctxglobaltemplates ctx)
                (get ctxfilecache ctx) (get ctxmrediscache ctx) pool (cronMailNoreplyAddress cronConf)
                (cronConsumerSigningMaxJobs cronConf)
          )
        , ( "document extending"
          , runConsumerWithIdleSignal . modTimeout
            $ documentExtendingConsumer (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                (get ctxglobaltemplates ctx) (get ctxfilecache ctx) (get ctxmrediscache ctx) pool (cronConsumerExtendingMaxJobs cronConf)
          )
        , ( "api callbacks"
          , runConsumerWithIdleSignal . modTimeout
            $ documentAPICallback {-runCronEnv-} id (cronConsumerAPICallbackMaxJobs cronConf)
          )
        , ( "amazon file upload"
          , runConsumerWithIdleSignal . modTimeout
            $ amazonUploadConsumer (cronAmazonConfig cronConf) pool (cronConsumerAmazonMaxJobs cronConf)
          )
        , ( "cron"
          , runConsumerWithIdleSignal . modTimeout
            $ cronConsumer cronConf reqManager {-mmixpanel-} Nothing {-mplanhat-} Nothing
                {-runCronEnv-} id {-runDB-} id (cronConsumerCronMaxJobs cronConf)
          )
        ]
      cronEnvData = CronEnv.CronEnv (cronSalesforceConf cronConf) (get ctxglobaltemplates ctx)
                      (cronMailNoreplyAddress cronConf)
      amazonCfg   = AmazonConfig (cronAmazonConfig cronConf) (get ctxfilecache ctx) (get ctxmrediscache ctx)

      finalizeRunner ((label, runner), idleSignal) =
        finalize (localDomain label $ runner pool idleSignal)



  (idleSignals, idleStatuses) <- liftIO . atomically $ do
    sigs <- replicateM (length cronPartRunners) $ newEmptyTMVar
    stats <- newTVar $ replicate (length cronPartRunners) False
    return (sigs, stats)

  -- To simplify things, runDB and runCronEnv requirements are added to the TestEnv. So then runDB and runCronEnv can be just "id".
  (\m -> runReaderT m cronEnvData)
    . runAmazonMonadT amazonCfg
    . foldr1 (.) (finalizeRunner <$> (zip cronPartRunners idleSignals))
    $ whileM_ (not <$> allSignalsTrue idleSignals idleStatuses) $ return ()

allSignalsTrue :: (MonadIO m) => [TMVar Bool] -> TVar [Bool] -> m Bool
allSignalsTrue idleSignals idleStatuses = liftIO . atomically $ do
  (idx, isIdle) <- takeAnyMVar idleSignals
  modifyTVar idleStatuses (\ss -> take idx ss ++ [isIdle] ++ drop (idx+1) ss)
  all id <$> readTVar idleStatuses

takeAnyMVar :: [TMVar a] -> STM (Int, a)
takeAnyMVar = foldr1 orElse . fmap (\(idx,t) -> liftM (idx,) $ takeTMVar t) . zip [0..]
