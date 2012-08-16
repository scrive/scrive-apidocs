module Cron (
    CronInfo
  , forkCron
  , stopCron
  ) where

import Control.Arrow
import Control.Monad
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as TG

newtype CronInfo = CronInfo (ThreadId, MVar Bool)

-- | Given an action f and a number of seconds t, cron will execute
-- f every t seconds with the first execution t seconds after cron is called.
forkCron :: TG.ThreadGroup -> Integer -> IO () -> IO CronInfo
forkCron tg seconds action = do
  ctrl <- newEmptyMVar
  tid <- forkIO $ controller ctrl
  return $ CronInfo (tid, ctrl)
  where
    controller ctrl = do
      _ <- TG.forkIO tg worker
      let (times::Int, rest::Int) = fromInteger *** fromInteger $ (seconds * 1000000) `divMod` (fromIntegral(maxBound::Int)::Integer)
      forever $ do
        replicateM_ times $ threadDelay maxBound
        threadDelay rest
        tryPutMVar ctrl True
      where
        worker = do
          exec <- takeMVar ctrl
          case exec of
            True  -> action >> worker
            False -> return ()

-- | Stops given cron thread. Use that before calling wait with appropriate
-- ThreadGroup object.
stopCron :: CronInfo -> IO ()
stopCron (CronInfo (tid, ctrl)) = do
  killThread tid
  putMVar ctrl False
