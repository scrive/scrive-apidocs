module CronEnv (runScheduler) where

import Control.Monad.Base
import qualified Data.ByteString as BS
import qualified Database.Redis as R

import ActionQueue.Monad (ActionQueueT, runQueue)
import ActionQueue.Scheduler (SchedulerData(..))
import CronConf (CronConf, cronAmazonConfig, cronGuardTimeConf, cronMailNoreplyAddress, cronSalesforceConf)
import File.FileID (FileID)
import KontraPrelude
import Templates (KontrakcjaGlobalTemplates)
import qualified Amazon as AWS
import qualified MemCache

runScheduler :: MonadBase IO m
             => CronConf
             -> MemCache.MemCache FileID BS.ByteString
             -> Maybe R.Connection
             -> KontrakcjaGlobalTemplates
             -> ActionQueueT (AWS.AmazonMonadT m) SchedulerData a
             -> m a
runScheduler cronConf localCache globalCache templates x = do
  let amazoncfg     = AWS.AmazonConfig (cronAmazonConfig cronConf)
                      localCache globalCache
      schedulerData = SchedulerData (cronGuardTimeConf cronConf)
                      (cronSalesforceConf cronConf) templates
                      (cronMailNoreplyAddress cronConf)
  AWS.runAmazonMonadT amazoncfg $ runQueue schedulerData x
