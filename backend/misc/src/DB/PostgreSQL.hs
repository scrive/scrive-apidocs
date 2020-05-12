{-# LANGUAGE DataKinds #-}
module DB.PostgreSQL where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Data.Pool
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Model.CompositeType
import Log

type BasicConnectionSource = ConnectionSource '[MonadBase IO, MonadMask]
type TrackedConnectionSource = ConnectionSource '[MonadBase IO, MonadMask, MonadLog]

newtype ConnectionTracker = ConnectionTracker {
    unConnectionTracker :: forall m. MonadLog m => Int -> Int -> m ()
  }

maxConnectionTracker :: Int -> ConnectionTracker
maxConnectionTracker maxConnections = ConnectionTracker $ \allocatedNow availableNow ->
  do
    when (allocatedNow == maxConnections && availableNow == 0) $ do
      logAttention "Limit of available database connections reached"
        $ object ["allocated" .= allocatedNow]

detailedConnectionTracker :: ConnectionTracker
detailedConnectionTracker = ConnectionTracker $ \allocatedNow availableNow -> do
  logInfo "Database connection acquired"
    . object
    $ ["allocated" .= allocatedNow, "available" .= availableNow]

----------------------------------------

pgConnSettings :: Text -> [CompositeType] -> ConnectionSettings
pgConnSettings dbconf ctypes = defaultConnectionSettings
  { csConnInfo   = dbconf
  , csComposites = map (unRawSQL . ctName) ctypes
  }

createPoolSource
  :: ConnectionSettings -> Int -> IO (ConnectionTracker -> TrackedConnectionSource)
createPoolSource cs maxConnections = do
  pool <- createPool (connect cs)
                     disconnect
                     1  -- number of subpools, we do not need that functionality
                     10 -- connection linger time after returned to pool
                     maxConnections
  return $ \tracker -> ConnectionSource $ ConnectionSourceM
    { withConnection = withResource' tracker pool . (clearStats >=>)
    }
  where
    withResource'
      :: (MonadBase IO m, MonadLog m, MonadMask m)
      => ConnectionTracker
      -> Pool Connection
      -> (Connection -> m a)
      -> m a
    withResource' (ConnectionTracker tracker) pool m = mask $ \restore -> do
      (resource    , local       ) <- liftBase $ takeResource pool
      (allocatedNow, availableNow) <- internalPoolState local
      tracker allocatedNow availableNow
      ret <- restore (m resource) `onException` do
        liftBase (destroyResource pool local resource)
      liftBase $ putResource local resource
      return ret

    internalPoolState local =
      liftBase
        $   (,)
        <$> readTVarIO (inUse local)
        <*> (length <$> readTVarIO (entries local))

    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ (\cd -> cd { cdStats = ConnectionStats 0 0 0 0 }) <$> mconn
      return conn

withPostgreSQL :: (MonadBase IO m, MonadMask m) => ConnectionSourceM m -> DBT m a -> m a
withPostgreSQL cs = runDBT cs defaultTransactionSettings
