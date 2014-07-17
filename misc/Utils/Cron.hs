module Utils.Cron (
    CronInfo
  , forkCron_
  , forkCron
  , stopCron
  , withCronJobs
  ) where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread.Group as TG
import qualified Control.Exception.Lifted as E
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Log

newtype CronInfo = CronInfo (TVar (WorkerState, Command))

data WorkerState = Waiting | Running
  deriving Eq

data Command = Continue | Finish
  deriving Eq

-- | Given an action f and a number of seconds t, cron will execute f
-- every t seconds.  A flag determines if the first execution starts
-- immediately, or after t seconds.
--
-- Returned value should be used for ordering given action to stop executing
-- by passing it to stopCron. After that you can call wait on passed ThreadGroup
-- to wait until running action (if there is one) finishes safely.
--
-- Note that action may use supplied function to enter interruptible state, ie.
-- call to stopCron issued when action is running, but within interruptible section
-- will interrupt the action immediately (this is particurarly useful when action
-- has to block for a period of time and it's safe to interrupt it in such state).
forkCron :: Bool -- ^ If True, wait t seconds before starting the action.
         -> String -> Integer -> ((forall a. IO a -> IO a) -> IO ()) -> TG.ThreadGroup -> IO CronInfo
forkCron waitfirst name seconds action tg = do
  vars@(ctrl, _) <- atomically ((,) <$> newTVar (Waiting, Continue) <*> newTVar False)
  _ <- forkIO $ controller vars
  return $ CronInfo ctrl
  where
    controller (ctrl, int) = do
      (wid, _) <- TG.forkIO tg worker
      let (times::Int, rest::Int) = fromInteger *** fromInteger $ (seconds * 1000000) `divMod` (fromIntegral(maxBound::Int)::Integer)
      let wait = do
            replicateM_ times $ threadDelay maxBound
            threadDelay rest
            start
          start = do
            -- start worker...
            atomically . modifyTVar' ctrl $ first (const Running)
            -- ... and wait until it's done (unless it's in interruptible
            -- section and we want to finish, then we just kill it).
            kill_worker <- atomically $ do
              (ws, cmd) <- readTVar ctrl
              interruptible <- readTVar int
              let kill_worker = cmd == Finish && interruptible
              when (ws == Running && not kill_worker) retry
              return kill_worker
            case kill_worker of
              True  -> do
                killThread wid
                -- mark thread as not running, so STM transaction
                -- in release function can pass
                atomically $ modifyTVar' ctrl $ first (const Waiting)
              False -> wait
      if waitfirst then wait else start
      where
        release :: IO a -> IO a
        release m = do
          atomically $ writeTVar int True
          E.finally m . atomically $ do
            (ws, cmd) <- readTVar ctrl
            -- if worker was ordered to finish, just wait until controller
            -- kills it, do not re-enter noninterruptible section again.
            when (ws == Running && cmd == Finish) retry
            writeTVar int False
        worker = do
          st <- atomically $ do
            st@(ws, cmd) <- readTVar ctrl
            when (ws == Waiting && cmd == Continue) retry
            return st
          case st of
            (Running, Continue) -> do
              action release `E.catch` \(e::E.SomeException) ->
                liftBase $ Log.attentionIO ("forkCron: exception caught in thread " ++ name ++ ": " ++ show e) (return ())
              atomically . modifyTVar' ctrl $ first (const Waiting)
              worker
            (_, Finish) -> liftBase $ Log.mixlogIO ("forkCron: finishing " ++ name ++ "...") (return ())
            (Waiting, Continue) -> liftBase $ Log.attentionIO ("forkCron: (Waiting, Continue) after (/= (Waiting, Continue)) condition. Something bad happened, exiting.") (return ())

-- | Same as forkCron, but there is no way to make parts
-- of passed action interruptible
forkCron_ :: Bool -> String -> Integer -> IO () -> TG.ThreadGroup -> IO CronInfo
forkCron_ waitfirst name seconds action = forkCron waitfirst name seconds (\_ -> action)

-- | Stops given cron thread. Use that before calling wait with appropriate
-- ThreadGroup object.
stopCron :: (MonadBase IO m) => CronInfo -> m ()
stopCron (CronInfo ctrl) = liftBase (atomically . modifyTVar' ctrl $ second (const Finish))

-- | Start a list of jobs in a local thread group, then perform an action, and finally stop the jobs and wait for the thread group.
withCronJobs :: (MonadBaseControl IO m) => [TG.ThreadGroup -> m CronInfo] -> ((TG.ThreadGroup, [CronInfo]) -> m a) -> m a
withCronJobs jobs = E.bracket start stop where
  start = do
    tg <- liftBase TG.new
    cil <- sequence (map ($ tg) jobs)
    return (tg, cil)
  stop (tg, cil) = do
    mapM_ stopCron cil
    liftBase (TG.wait tg)
