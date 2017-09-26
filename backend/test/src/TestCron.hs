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
runTestCronUntilIdle ctx = runTestCronPartsUntilIdle ctx [True, True, True, True, True, True]

runTestCronPartsUntilIdle :: Context -> [Bool] -> TestEnv ()
runTestCronPartsUntilIdle Context{..} enabled = do
  ConnectionSource pool <- asks teConnSource
  -- Will not be used, because Planhat is not configured when testing, but it is a parameter for cronConsumer.
  reqManager <- newTlsManager

  let docSealing   = documentSealing (cronAmazonConfig cronConf)
        (cronGuardTimeConf cronConf) ctxglobaltemplates ctxfilecache ctxmrediscache pool (cronMailNoreplyAddress cronConf)
        (cronConsumerSealingMaxJobs cronConf)
      docSigning   = documentSigning (cronAmazonConfig cronConf)
        (cronGuardTimeConf cronConf) (cronCgiGrpConfig cronConf)
        ctxglobaltemplates ctxfilecache ctxmrediscache pool (cronMailNoreplyAddress cronConf) (cronConsumerSigningMaxJobs cronConf)
      docExtending = documentExtendingConsumer (cronAmazonConfig cronConf)
        (cronGuardTimeConf cronConf) ctxglobaltemplates ctxfilecache ctxmrediscache pool (cronConsumerExtendingMaxJobs cronConf)
      amazonFileUpload = amazonUploadConsumer (cronAmazonConfig cronConf) pool (cronConsumerAmazonMaxJobs cronConf)

      apiCallbacks = documentAPICallback {-runCronEnv-} id (cronConsumerAPICallbackMaxJobs cronConf)
      cron = cronConsumer cronConf reqManager {-mmixpanel-} Nothing {-mplanhat-} Nothing {-runCronEnv-} id {-runDB-} id (cronConsumerCronMaxJobs cronConf)

  idleSignals <- forM [1..6::Int] . const . liftIO . atomically $ newEmptyTMVar
  idleStatuses :: TVar [Bool] <- liftIO . atomically . newTVar . map not $ enabled

  let cronEnvData = CronEnv.CronEnv (cronSalesforceConf cronConf) ctxglobaltemplates
                      (cronMailNoreplyAddress cronConf)
      amazonCfg   = AmazonConfig (cronAmazonConfig cronConf) ctxfilecache ctxmrediscache

  -- To simplify things, runDB and runCronEnv requirements are added to the TestEnv. So then runDB and runCronEnv can be just "id".
  (\m -> runReaderT m cronEnvData)
    . runAmazonMonadT amazonCfg
    . ifnot (enabled !! 0) id (finalize (localDomain "document sealing"   $ runConsumerWithIdleSignal  (modTimeout docSealing      ) pool (idleSignals !! 0)))
    . ifnot (enabled !! 1) id (finalize (localDomain "document signing"   $ runConsumerWithIdleSignal  (modTimeout docSigning      ) pool (idleSignals !! 1)))
    . ifnot (enabled !! 2) id (finalize (localDomain "document extending" $ runConsumerWithIdleSignal  (modTimeout docExtending    ) pool (idleSignals !! 2)))
    . ifnot (enabled !! 3) id (finalize (localDomain "api callbacks"      $ runConsumerWithIdleSignal  (modTimeout apiCallbacks    ) pool (idleSignals !! 3)))
    . ifnot (enabled !! 4) id (finalize (localDomain "amazon file upload" $ runConsumerWithIdleSignal  (modTimeout amazonFileUpload) pool (idleSignals !! 4)))
    . ifnot (enabled !! 5) id (finalize (localDomain "cron"               $ runConsumerWithIdleSignal  (modTimeout cron            ) pool (idleSignals !! 5)))
    $ whileM_ (not <$> allConsumersAreIdle idleSignals idleStatuses) $ return ()

  where
    ifnot True  _a  b = b
    ifnot False  a _b = a

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

    allConsumersAreIdle idleSignals idleStatuses = liftIO . atomically $ do
        (idx, isIdle) <- takeAnyMVar idleSignals
        modifyTVar idleStatuses (\ss -> take idx ss ++ [isIdle] ++ drop (idx+1) ss)
        all id <$> readTVar idleStatuses

    takeAnyMVar :: [TMVar a] -> STM (Int, a)
    takeAnyMVar = foldr1 orElse . fmap (\(idx,t) -> liftM (idx,) $ takeTMVar t) . zip [0..]
