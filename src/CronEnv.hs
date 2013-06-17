module CronEnv (runScheduler) where

import ActionQueue.Monad (ActionQueueT, runQueue)
import ActionQueue.Scheduler (SchedulerData(..))
import qualified Amazon as AWS
import AppConf (AppConf, amazonConfig)
import qualified Data.ByteString as BS
import Control.Concurrent (MVar)
import Control.Monad.Trans (MonadIO)
import File.FileID (FileID)
import qualified MemCache
import Data.Time (UTCTime)
import Templates (KontrakcjaGlobalTemplates)

runScheduler :: MonadIO m => AppConf -> MemCache.MemCache FileID BS.ByteString
                          -> MVar (UTCTime, KontrakcjaGlobalTemplates) -> ActionQueueT (AWS.AmazonMonadT m) SchedulerData a -> m a
runScheduler appConf filecache templates x = do
  let amazoncfg = AWS.AmazonConfig (amazonConfig appConf) filecache
  AWS.runAmazonMonadT amazoncfg $ runQueue (SchedulerData appConf templates) x
