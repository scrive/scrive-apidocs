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

-- | Run the consumer and perform other action. Cleaning
-- up takes place no matter whether supplied monadic action
-- exits normally or throws an exception.
withConsumer
  :: (MonadBaseControl IO m, MonadMask m, Show idx, ToSQL idx)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> Logger m
  -> m a
  -> m a
withConsumer cc cs logger action = do
  finisher <- newEmptyMVar
  flip finally (tryTakeMVar finisher >>= maybe (return ()) id) $ do
    putMVar finisher =<< runConsumer cc cs logger
    action

-- | Run the consumer. The purpose of the returned monadic action
-- is to wait for currently processed jobs and clean up. Use
-- 'withConsumer' instead unless you know what you're doing.
runConsumer
  :: (MonadBaseControl IO m, MonadMask m, MonadBase IO n, MonadMask n, Show idx, ToSQL idx)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> Logger m
  -> m (n ())
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

-- | Spawn a thread that generates signals for the
-- dispatcher to probe the database for incoming jobs.
spawnListener
  :: (MonadBaseControl IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> MVar ()
  -> m ThreadId
spawnListener cc cs semaphore = forkP "listener" $ case ccNotificationChannel cc of
  Just chan -> runDBT cs ts . bracket_ (listen chan) (unlisten chan) . forever $ do
    -- If there are many notifications, we need to collect them
    -- as soon as possible, because they are stored in memory by
    -- libpq. They are also not squashed, so we perform the
    -- squashing ourselves with the help of MVar ().
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

-- | Spawn a thread that monitors working consumers
-- for activity and periodically updates its own.
spawnMonitor
  :: (MonadBaseControl IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> DomainLogger m
  -> ConsumerID
  -> m ThreadId
spawnMonitor ConsumerConfig{..} cs logger cid = forkP "monitor" . forever $ do
  n <- runDBT cs ts $ do
    -- Update last_activity of the consumer.
    ok <- runSQL01 $ smconcat [
        "UPDATE" <+> raw ccConsumersTable
      , "SET last_activity = now()"
      , "WHERE id =" <?> cid
      , "  AND name =" <?> unRawSQL ccJobsTable
      ]
    when (not ok) $ do
      lift . logger $ "consumer" <+> show cid <+> "is not registered"
      throwM ThreadKilled
    -- Remove all inactive (presumably dead) consumers.
    runSQL $ smconcat [
        "WITH inactive AS ("
      , "  DELETE FROM" <+> raw ccConsumersTable
      , "  WHERE last_activity +" <?> iminutes 1 <+> "<= now()"
      , "    AND name =" <?> unRawSQL ccJobsTable
      , "  RETURNING id"
      , ")"
      -- Reset reserved jobs manually, do not rely
      -- on the foreign key constraint to do its job.
      , "UPDATE" <+> raw ccJobsTable
      , "SET reserved_by = NULL"
      , "WHERE reserved_by IN (SELECT id FROM inactive)"
      ]
  when (n > 0) $ do
    logger $ "unregistered" <+> show n <+> "inactive consumers"
  liftBase . threadDelay $ 30 * 1000000 -- wait 30 seconds
  where
    ts = def {
      tsIsolationLevel = ReadCommitted
    , tsPermissions = ReadWrite
    }

-- | Spawn a thread that reserves and processes jobs.
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
      -- PostgreSQL doesn't seem to handle very high amount of
      -- concurrent transactions that modify multiple rows in
      -- the same table well (see updateJobs) and sometimes (very
      -- rarely though) ends up in a deadlock. It doesn't matter
      -- much though, we just restart the transaction in such case.
      $ \e _ -> qeErrorCode e == DeadlockDetected
    , tsPermissions = ReadWrite
    }

    loop :: Int -> m ()
    loop limit = do
      (batch, batchSize) <- reserveJobs limit
      when (batchSize > 0) $ do
        logger $ "processing batch of size" <+> show batchSize
        -- Update runningJobs before forking so that we can
        -- adjust maxBatchSize appropriately later. Also, any
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
      -- Decode lazily as we want the transaction to be as short as possible.
      (, n) . F.toList . fmap ccJobFetcher <$> queryResult
      where
        reservedJobs :: SQL
        reservedJobs = smconcat [
            "SELECT id FROM" <+> raw ccJobsTable
            -- Converting id to text and hashing it may seem silly,
            -- especially when we're dealing with integers in the first
            -- place, but even in such case the overhead is small enough
            -- (converting 100k integers to text and hashing them takes
            -- around 15 ms on i7) to be worth the generality.
            -- Also: after PostgreSQL 9.5 is released, we can use SELECT
            -- FOR UPDATE SKIP LOCKED instead of advisory locks (see
            -- http://michael.otacoo.com/postgresql-2/postgres-9-5-feature-highlight-skip-locked-row-level/
            -- for more details). Also, note that even if IDs of two
            -- pending jobs produce the same hash, it just means that
            -- in the worst case they will be processed by the same consumer.
          , "WHERE pg_try_advisory_xact_lock(" <?> unRawSQL ccJobsTable <> "::regclass::integer, hashtext(id::text))"
          , "  AND reserved_by IS NULL"
          , "  AND run_at IS NOT NULL"
          , "  AND run_at <= now()"
          , "LIMIT" <?> limit
          ]

    -- | Spawn each job in a separate thread.
    startJob :: job -> m (job, m (T.Result Result))
    startJob job = do
      (_, joinFork) <- T.fork $ ccProcessJob job
      return (job, joinFork)

    -- | Wait for all the jobs and collect their results.
    joinJob :: (job, m (T.Result Result)) -> m (idx, Result)
    joinJob (job, joinFork) = joinFork >>= \case
      Right result -> return (ccJobIndex job, result)
      Left ex -> do
        action <- ccOnException job
        logger $ "unexpected exception" <+> show ex <+> "caught while processing job" <+> show (ccJobIndex job) <> ", action:" <+> show action
        return (ccJobIndex job, Failed action)

    -- | Update status of the jobs.
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
        retryToSQL (Left int) ids =
          ("WHEN id = ANY(" <?> Array1 ids <+> ") THEN now() +" <?> int :)
        retryToSQL (Right time) ids =
          ("WHEN id = ANY(" <?> Array1 ids <+> ") THEN" <?> time :)

        retries = foldr step M.empty $ map f updates
          where
            f (idx, result) = case result of
              Ok     action -> (idx, action)
              Failed action -> (idx, action)

            step (idx, action) iretries = case action of
              MarkProcessed  -> iretries
              RetryAfter int -> M.insertWith (++) (Left int) [idx] iretries
              RetryAt time   -> M.insertWith (++) (Right time) [idx] iretries
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
