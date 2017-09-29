module TestCron (runTestCronUntilIdle) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Data.Maybe
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
import KontraPrelude hiding (All)
import TestKontra
import qualified CronEnv

runTestCronUntilIdle :: Context -> TestEnv ()
runTestCronUntilIdle  Context{..} = do
  -- This is intented to run as part of a test and tests are usually one big transaction.
  -- Running consumers spawns additional processes with their own DB transactions,
  -- which do not see changes of the test transaction ... unless there is a commit.
  commit
  ConnectionSource pool <- asks teConnSource
  -- Will not be used, because Planhat is not configured when testing, but it is a parameter for cronConsumer.
  reqManager <- newTlsManager

  let -- for testing, one of each is sufficient
      cronConf = def
        { cronConsumerCronMaxJobs        = 1
        , cronConsumerSealingMaxJobs     = 1
        , cronConsumerSigningMaxJobs     = 1
        , cronConsumerExtendingMaxJobs   = 1
        , cronConsumerAPICallbackMaxJobs = 1
        , cronConsumerAmazonMaxJobs      = 1
        }

      -- make timeouts small, so that the test runs faster
      modTimeout = \c -> c { ccNotificationTimeout = 100 * 1000 }
      cronPartRunners =
        [ ( "document sealing"
          , runConsumerWithIdleSignal . modTimeout
            $ documentSealing (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                ctxglobaltemplates ctxfilecache ctxmrediscache pool
                (cronMailNoreplyAddress cronConf) (cronConsumerSealingMaxJobs cronConf)
          )
        , ( "document signing"
          , runConsumerWithIdleSignal . modTimeout
            $ documentSigning (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                (cronCgiGrpConfig cronConf) ctxglobaltemplates ctxfilecache ctxmrediscache
                pool (cronMailNoreplyAddress cronConf) (cronConsumerSigningMaxJobs cronConf)
          )
        , ( "document extending"
          , runConsumerWithIdleSignal . modTimeout
            $ documentExtendingConsumer (cronAmazonConfig cronConf) (cronGuardTimeConf cronConf)
                ctxglobaltemplates ctxfilecache ctxmrediscache pool (cronConsumerExtendingMaxJobs cronConf)
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
      cronEnvData = CronEnv.CronEnv (cronSalesforceConf cronConf) ctxglobaltemplates
                      (cronMailNoreplyAddress cronConf)
      amazonCfg   = AmazonConfig (cronAmazonConfig cronConf) ctxfilecache ctxmrediscache

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
