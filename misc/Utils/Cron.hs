module Utils.Cron where

import Control.Arrow
import Control.Concurrent
import Control.Monad.State

-- | Given an action f and a number of seconds t, cron will execute
-- f every t seconds with the first execution t seconds after cron is called.
-- cron does not spawn a new thread.
cron :: Integer -> IO () -> IO a
cron seconds action = do
  let (times::Int, rest::Int) = fromInteger *** fromInteger $ (seconds * 1000000) `divMod` (fromIntegral(maxBound::Int)::Integer)
  forever $ do
    replicateM_ times $ threadDelay maxBound
    threadDelay rest
    action
