module JobQueue.Components (
    withConsumer
  , runConsumer
  , spawnListener
  , spawnMonitor
  , spawnDispatcher
  ) where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.STM hiding (atomically)
import Control.Exception (AsyncException(ThreadKilled))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Monoid
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import qualified Control.Concurrent.STM as STM
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

import JobQueue.Config
import JobQueue.Consumer
import JobQueue.Utils
import qualified Control.Concurrent.Thread.Group.Lifted as TG
import qualified Control.Concurrent.Thread.Lifted as T

-- local logger types
type Logger m = String -> String -> m ()
type DomainLogger m = String -> m ()

withConsumer
  :: (MonadBaseControl IO m, MonadMask m, Show idx, ToSQL idx)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> Logger m
  -> m ()
  -> m ()
withConsumer cc cs logger action = do
  finisher <- newEmptyMVar
  flip finally (tryTakeMVar finisher >>= maybe (return ()) id) $ do
    putMVar finisher =<< runConsumer cc cs logger
    action

runConsumer
  :: (MonadBaseControl IO m, MonadMask m, Show idx, ToSQL idx)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> Logger m
  -> m (m ())
runConsumer cc cs logger = do
  semaphore <- newMVar ()
  batches <- TG.new
  runningJobs <- atomically $ newTVar 0

  cid <- registerConsumer cc cs
  listener <- spawnListener cc cs semaphore
  monitor <- spawnMonitor cc cs (logger $ "Monitor" <+> show cid) cid
  dispatcher <- spawnDispatcher cc cs (logger $ "Dispatcher" <+> show cid) cid semaphore batches runningJobs

  return $ do
    stopExecution listener
    stopExecution dispatcher
    TG.wait batches
    stopExecution monitor
    unregisterConsumer cc cs cid

spawnListener
  :: (MonadBaseControl IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> MVar ()
  -> m ThreadId
spawnListener cc cs semaphore = forkP "listener" $ case ccNotificationChannel cc of
  Just chan -> runDBT cs ts . bracket_ (listen chan) (unlisten chan) . forever $ do
    void . getNotification $ ccNotificationTimeout cc
    tryPutMVar semaphore ()
  Nothing -> forever . liftBase $ do
    threadDelay $ ccNotificationTimeout cc
    tryPutMVar semaphore ()
  where
    ts = def {
      tsAutoTransaction = False
    , tsIsolationLevel = ReadCommitted
    , tsPermissions = ReadOnly
    }

spawnMonitor
  :: (MonadBaseControl IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> DomainLogger m
  -> ConsumerID
  -> m ThreadId
spawnMonitor ConsumerConfig{..} cs logger cid = forkP "monitor" . forever $ do
  n <- runDBT cs ts $ do
    -- update last_activity of the worker
    ok <- runSQL01 $ smconcat [
        "UPDATE" <+> raw ccConsumersTable
      , "SET last_activity = now()"
      , "WHERE id =" <?> cid
      ]
    when (not ok) $ do
      lift . logger $ "consumer" <+> show cid <+> "is not registered"
      throwM ThreadKilled
    -- remove all inactive (presumably dead) workers
    runSQL $ smconcat [
        "DELETE FROM" <+> raw ccConsumersTable
      , "WHERE last_activity +" <?> iminutes 1 <+> "<= now()"
      ]
  when (n > 0) $ do
    logger $ "unregistered" <+> show n <+> "inactive workers"
  liftBase . threadDelay $ 30 * 1000000 -- wait 30 seconds
  where
    ts = def {
      tsIsolationLevel = ReadCommitted
    , tsPermissions = ReadWrite
    }

spawnDispatcher
  :: forall m idx job. (MonadBaseControl IO m, MonadMask m, Show idx, ToSQL idx)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> DomainLogger m
  -> ConsumerID
  -> MVar ()
  -> TG.ThreadGroup
  -> TVar Int
  -> m ThreadId
spawnDispatcher ConsumerConfig{..} cs logger cid semaphore batches runningJobs =
  forkP "dispatcher" . forever $ do
    void $ takeMVar semaphore
    loop 1
  where
    ts = def {
      tsIsolationLevel = ReadCommitted
    , tsRestartPredicate = Just . RestartPredicate
      -- postgresql doesn't seem to handle very high amount of
      -- concurrent transactions that modify multiple rows in
      -- the same table well (see updateJobs) and sometimes (very
      -- rarely though) ends up in a deadlock. it doesn't matter
      -- much though, we just restart the transaction in such case.
      $ \e _ -> qeErrorCode e == DeadlockDetected
    , tsPermissions = ReadWrite
    }

    loop :: Int -> m ()
    loop limit = do
      (batch, batchSize) <- reserveJobs limit
      when (batchSize > 0) $ do
        logger $ "processing batch of size" <+> show batchSize
        -- update runningJobs before forking so that we can
        -- adjust maxBatchSize appropriately later. also, any
        -- exception thrown within fork is propagated down to
        -- parent thread, so we don't need to worry about
        -- locking jobs infinitely if that happens.
        atomically $ modifyTVar' runningJobs (+batchSize)
        void . gforkP batches "batch processor" $ do
          mapM startJob batch >>= mapM joinJob >>= updateJobs
          atomically $ modifyTVar' runningJobs (subtract batchSize)
        when (batchSize == limit) $ do
          maxBatchSize <- atomically $ do
            jobs <- readTVar runningJobs
            when (jobs >= ccMaxRunningJobs) retry
            return $ ccMaxRunningJobs - jobs
          loop $ min maxBatchSize (2*limit)

    reserveJobs :: Int -> m ([job], Int)
    reserveJobs limit = runDBT cs ts $ do
      n <- runSQL $ smconcat [
          "UPDATE" <+> raw ccJobsTable <+> "SET"
        , "  reserved_by =" <?> cid
        , ", attempts = CASE"
        , "    WHEN finished_at IS NULL THEN attempts + 1"
        , "    ELSE 1"
        , "  END"
        , "WHERE id IN (" <> reservedJobs <> ")"
        , "RETURNING" <+> mintercalate ", " ccJobSelectors
        ]
      (, n) . F.toList . fmap ccJobFetcher <$> queryResult
      where
        reservedJobs :: SQL
        reservedJobs = smconcat [
            "SELECT id FROM" <+> raw ccJobsTable
          , "WHERE pg_try_advisory_xact_lock(" <?> unRawSQL ccJobsTable <> "::regclass::integer, hashtext(id::text))"
          , "  AND reserved_by IS NULL"
          , "  AND run_at IS NOT NULL"
          , "  AND run_at <= now()"
          , "LIMIT" <?> limit
          ]

    startJob :: job -> m (job, m (T.Result Result))
    startJob job = do
      (_, joinFork) <- T.fork $ ccProcessJob job
      return (job, joinFork)

    joinJob :: (job, m (T.Result Result)) -> m (idx, Result)
    joinJob (job, joinFork) = joinFork >>= \case
      Right result -> return (ccJobIndex job, result)
      Left ex -> do
        let action = ccOnException job
        logger $ "unexpected exception" <+> show ex <+> "caught while processing job" <+> show (ccJobIndex job) <> ", action:" <+> show action
        return (ccJobIndex job, Failed action)

    updateJobs :: [(idx, Result)] -> m ()
    updateJobs results = runDBT cs ts $ do
      runSQL_ $ smconcat [
          "WITH removed AS ("
        , "  DELETE FROM" <+> raw ccJobsTable
        , "  WHERE id = ANY(" <?> Array1 deletes <+> ")"
        , ")"
        , "UPDATE" <+> raw ccJobsTable <+> "SET"
        , "  reserved_by = NULL"
        , ", run_at = CASE"
        , "    WHEN FALSE THEN run_at"
        ,      smconcat $ M.foldrWithKey retryToSQL [] retries
        , "    ELSE NULL" -- processed
        , "  END"
        , ", finished_at = CASE"
        , "    WHEN id = ANY(" <?> Array1 successes <+> ") THEN now()"
        , "    ELSE NULL"
        , "  END"
        , "WHERE id = ANY(" <?> Array1 (map fst updates) <+> ")"
        ]
      where
        retryToSQL int ids =
          ("WHEN id = ANY(" <?> Array1 ids <+> ") THEN now() +" <?> int :)

        retries = foldr step M.empty $ map f updates
          where
            f (idx, result) = case result of
              Ok     action -> (idx, action)
              Failed action -> (idx, action)

            step (idx, action) iretries = case action of
              MarkProcessed  -> iretries
              RetryAfter int -> M.insertWith (++) int [idx] iretries
              Remove         -> error "updateJobs: Remove should've been filtered out"

        successes = foldr step [] updates
          where
            step (idx, Ok     _) acc = idx : acc
            step (_,   Failed _) acc =       acc

        (deletes, updates) = foldr step ([], []) results
          where
            step job@(idx, result) (ideletes, iupdates) = case result of
              Ok     Remove -> (idx : ideletes, iupdates)
              Failed Remove -> (idx : ideletes, iupdates)
              _             -> (ideletes, job : iupdates)

----------------------------------------

atomically :: MonadBase IO m => STM a -> m a
atomically = liftBase . STM.atomically
