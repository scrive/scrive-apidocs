module CronEnv (runScheduler) where

import Control.Monad.Base
import qualified Data.ByteString as BS
import qualified Database.Redis as R

import ActionQueue.Monad (ActionQueueT, runQueue)
import ActionQueue.Scheduler (SchedulerData(..))
import CronConf (CronConf, amazonConfig, guardTimeConf, mailNoreplyAddress, salesforceConf)
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
  let amazoncfg     = AWS.AmazonConfig (amazonConfig cronConf)
                      localCache globalCache
      schedulerData = SchedulerData (guardTimeConf cronConf)
                      (salesforceConf cronConf) templates (mailNoreplyAddress cronConf)
  AWS.runAmazonMonadT amazoncfg $ runQueue schedulerData x
