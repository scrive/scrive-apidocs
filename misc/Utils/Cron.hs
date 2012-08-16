module Utils.Cron (
    CronInfo
  , forkCron
  , stopCron
  ) where

import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread.Group as TG
import qualified Control.Exception as E

import qualified Log

newtype CronInfo = CronInfo (ThreadId, TVar (WorkerState, Command))

data WorkerState = Waiting | Running
  deriving Eq

data Command = Continue | Finish
  deriving Eq

-- | Given an action f and a number of seconds t, cron will execute
-- f every t seconds with the first execution t seconds after cron is called.
forkCron :: TG.ThreadGroup -> String -> Integer -> IO () -> IO CronInfo
forkCron tg name seconds action = do
  ctrl <- atomically $ newTVar (Waiting, Continue)
  tid <- forkIO $ controller ctrl
  return $ CronInfo (tid, ctrl)
  where
    controller ctrl = do
      _ <- TG.forkIO tg worker
      let (times::Int, rest::Int) = fromInteger *** fromInteger $ (seconds * 1000000) `divMod` (fromIntegral(maxBound::Int)::Integer)
      forever $ do
        replicateM_ times $ threadDelay maxBound
        threadDelay rest
        -- start worker...
        atomically . modifyTVar ctrl $ first (const Running)
        -- ... and wait until it's finished
        atomically $ do
          (ws, _) <- readTVar ctrl
          when (ws == Running) retry
      where
        worker = do
          st <- atomically $ do
            st@(ws, cmd) <- readTVar ctrl
            when (ws == Waiting && cmd == Continue) retry
            return st
          case st of
            (Running, Continue) -> do
              action `E.catch` \(e::E.SomeException) ->
                Log.error ("forkCron: exception caught in thread " ++ name ++ ": " ++ show e)
              atomically . modifyTVar ctrl $ first (const Waiting)
              worker
            (_, Finish) -> Log.server $ "forkCron: finishing " ++ name ++ "..."
            (Waiting, Continue) -> Log.error "forkCron: (Waiting, Continue) after (/= (Waiting, Continue)) condition. Something bad happened, exiting."

-- | Stops given cron thread. Use that before calling wait with appropriate
-- ThreadGroup object.
stopCron :: CronInfo -> IO ()
stopCron (CronInfo (tid, ctrl)) = do
  killThread tid
  atomically . modifyTVar ctrl $ second (const Finish)
