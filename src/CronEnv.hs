module CronEnv (runScheduler) where

import Control.Concurrent (MVar)
import Control.Monad.Trans (MonadIO)
import Data.Time (UTCTime)
import qualified Data.ByteString as BS

import ActionQueue.Monad (ActionQueueT, runQueue)
import ActionQueue.Scheduler (SchedulerData(..))
import AppConf (AppConf, amazonConfig)
import File.FileID (FileID)
import Templates (KontrakcjaGlobalTemplates)
import qualified Amazon as AWS
import qualified MemCache

runScheduler :: MonadIO m => AppConf -> MemCache.MemCache FileID BS.ByteString
                          -> MVar (UTCTime, KontrakcjaGlobalTemplates) -> ActionQueueT (AWS.AmazonMonadT m) SchedulerData a -> m a
runScheduler appConf filecache templates x = do
  let amazoncfg = AWS.AmazonConfig (amazonConfig appConf) filecache
  AWS.runAmazonMonadT amazoncfg $ runQueue (SchedulerData appConf templates) x
