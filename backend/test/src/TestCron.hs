module TestCron (runTestCronUntilIdle) where

import Control.Concurrent.STM
import Control.Monad.Loops
import Control.Monad.Reader
import Database.PostgreSQL.Consumers
import Log
import Network.HTTP.Client.TLS (newTlsManager)
import Optics (gview)

import Context
import Cron.Model
import CronConf
import DB
import Doc.API.Callback.Model
import Doc.Extending.Consumer
import Doc.Sealing.Consumer
import Doc.Signing.Consumer
import Purging.Files
import TestKontra
import qualified CronEnv

runTestCronUntilIdle :: Context -> TestEnv ()
runTestCronUntilIdle ctx = do
  -- This is intended to run as part of a test and tests are usually
  -- one big transaction. Running consumers spawns additional
  -- processes with their own DB transactions, which do not see
  -- changes of the test transaction ... unless there is a commit.
  commit
  ConnectionSource pool  <- gview #connSource
  pdfSealLambdaEnv       <- gview #pdfToolsLambdaEnv
  cronDBConfig           <- gview #cronDBConfig
  cronMonthlyInvoiceConf <- gview #cronMonthlyInvoice

  -- Will not be used, because Planhat is not configured when testing,
  -- but it is a parameter for cronConsumer.
  reqManager             <- newTlsManager

  let -- for testing, one of each is sufficient
    cronConf = CronConf
      { cronAmazonConfig               = unexpectedError "cronAmazonConfig undefined"
      , cronDBConfig                   = cronDBConfig
      , cronMaxDBConnections           = 100
      , cronRedisCacheConfig           = Nothing
      , cronLocalFileCacheSize         = 200000000
      , cronLogConfig                  = testLogConfig
      , cronGuardTimeConf              = testGTConf
      , cronCgiGrpConfig               = Nothing
      , cronMixpanelToken              = Nothing
      , cronNtpServers = [ (showt n) <> ".ubuntu.pool.ntp.org" | n :: Int <- [0 .. 3] ]
      , cronSalesforceConf             = Nothing
      , cronPlanhatConf                = Nothing
      , cronMonitoringConf             = Nothing
      , cronMailNoreplyAddress         = "noreply@scrive.com"
      , cronConsumerCronMaxJobs        = 1
      , cronConsumerSealingMaxJobs     = 1
      , cronConsumerSigningMaxJobs     = 1
      , cronConsumerExtendingMaxJobs   = 1
      , cronConsumerAPICallbackMaxJobs = 1
      , cronConsumerFilePurgingMaxJobs = 1
      , cronNetsSignConfig             = Nothing
      , cronPdfToolsLambdaConf = unexpectedError "cronPdfToolsLambdaConf undefined"
      , cronMonthlyInvoiceConf         = cronMonthlyInvoiceConf
      , cronStatsDConf                 = Nothing
      , cronEIDServiceConf             = Nothing
      }

    -- make timeouts small, so that the test runs faster
    modTimeout = \c -> c { ccNotificationTimeout = 100 * 1000 }
    cronPartRunners =
      [ ( "document sealing"
        , runConsumerWithIdleSignal . modTimeout $ documentSealing
          (cronGuardTimeConf cronConf)
          pdfSealLambdaEnv
          (ctx ^. #globalTemplates)
          pool
          (cronMailNoreplyAddress cronConf)
          (cronConsumerSealingMaxJobs cronConf)
        )
      , ( "document signing"
        , runConsumerWithIdleSignal . modTimeout $ documentSigning
          (cronGuardTimeConf cronConf)
          (cronCgiGrpConfig cronConf)
          (cronNetsSignConfig cronConf)
          (cronEIDServiceConf cronConf)
          (ctx ^. #globalTemplates)
          pool
          (cronMailNoreplyAddress cronConf)
          (cronConsumerSigningMaxJobs cronConf)
        )
      , ( "document extending"
        , runConsumerWithIdleSignal . modTimeout $ documentExtendingConsumer
          (cronGuardTimeConf cronConf)
          (ctx ^. #globalTemplates)
          pool
          (cronConsumerExtendingMaxJobs cronConf)
        )
      , ( "api callbacks"
        , runConsumerWithIdleSignal . modTimeout $ documentAPICallback {-runCronEnv-}identity
          (cronConsumerAPICallbackMaxJobs cronConf)
        )
      , ( "cron"
        , runConsumerWithIdleSignal . modTimeout $ cronConsumer
          cronConf
          reqManager {-mmixpanel-}Nothing
              {-mplanhat-}Nothing {-runCronEnv-}identity {-runDB-}identity
          (cronConsumerCronMaxJobs cronConf)
        )
      , ( "file purging"
        , runConsumerWithIdleSignal . modTimeout $ filePurgingConsumer
          pool
          (cronConsumerFilePurgingMaxJobs cronConf)
        )
      ]
    cronEnvData = CronEnv.CronEnv (cronSalesforceConf cronConf)
                                  (ctx ^. #globalTemplates)
                                  (cronMailNoreplyAddress cronConf)

    finalizeRunner ((label, runner), idleSignal) =
      finalize (localDomain label $ runner pool idleSignal)

  (idleSignals, idleStatuses) <- liftIO . atomically $ do
    sigs  <- replicateM (length cronPartRunners) $ newEmptyTMVar
    stats <- newTVar $ replicate (length cronPartRunners) False
    return (sigs, stats)

  -- To simplify things, runDB and runCronEnv requirements are added
  -- to the TestEnv. So then runDB and runCronEnv can be just "id".
  (\m -> runReaderT m cronEnvData)
    . foldr1 (.) (finalizeRunner <$> (zip cronPartRunners idleSignals))
    $ whileM_ (not <$> allSignalsTrue idleSignals idleStatuses)
    $ return ()

allSignalsTrue :: (MonadIO m) => [TMVar Bool] -> TVar [Bool] -> m Bool
allSignalsTrue idleSignals idleStatuses = liftIO . atomically $ do
  (idx, isIdle) <- takeAnyMVar idleSignals
  modifyTVar idleStatuses (\ss -> take idx ss <> [isIdle] <> drop (idx + 1) ss)
  all identity <$> readTVar idleStatuses

takeAnyMVar :: [TMVar a] -> STM (Int, a)
takeAnyMVar =
  foldr1 orElse . fmap (\(idx, t) -> liftM (idx, ) $ takeTMVar t) . zip [0 ..]
