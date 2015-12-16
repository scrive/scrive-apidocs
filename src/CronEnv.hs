module CronEnv (runScheduler) where

import Control.Monad.Base
import qualified Data.ByteString as BS

import ActionQueue.Monad (ActionQueueT, runQueue)
import ActionQueue.Scheduler (SchedulerData(..))
import AppConf (AppConf, amazonConfig)
import File.FileID (FileID)
import KontraPrelude
import Templates (KontrakcjaGlobalTemplates)
import qualified Amazon as AWS
import qualified MemCache

runScheduler :: MonadBase IO m
             => AppConf
             -> MemCache.MemCache FileID BS.ByteString
             -> KontrakcjaGlobalTemplates
             -> ActionQueueT (AWS.AmazonMonadT m) SchedulerData a
             -> m a
runScheduler appConf filecache templates x = do
  let amazoncfg = AWS.AmazonConfig (amazonConfig appConf) filecache
  AWS.runAmazonMonadT amazoncfg $ runQueue (SchedulerData appConf templates) x
