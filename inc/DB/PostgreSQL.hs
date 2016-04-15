module DB.PostgreSQL where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson.Types (Pair)
import Data.Pool
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection
import Log

import DB.Model.CompositeType
import KontraPrelude

newtype ConnectionTracker = ConnectionTracker { unConnectionTracker :: forall m. MonadBase IO m => Int -> Int -> m () }

maxConnectionTracker :: (forall m r. LogT m r -> m r) -> ConnectionTracker
maxConnectionTracker runLogger = ConnectionTracker $ \allocatedNow availableNow -> do
  when (allocatedNow == maxConnections && availableNow == 0) $ do
    liftBase . runLogger $ logAttention "Limit of available database connections reached" $ object [
        "allocated" .= allocatedNow
      ]

detailedConnectionTracker :: (forall m r. LogT m r -> m r) -> [Pair] -> ConnectionTracker
detailedConnectionTracker runLogger extraData = ConnectionTracker $ \allocatedNow availableNow -> do
  liftBase . runLogger . logInfo "Database connection acquired" . object $ [
      "allocated" .= allocatedNow
    , "available" .= availableNow
    ] ++ extraData

----------------------------------------

maxConnections :: Int
maxConnections = 100

pgConnSettings :: Text -> [CompositeType] -> ConnectionSettings
pgConnSettings dbconf ctypes = def {
  csConnInfo = encodeUtf8 dbconf
, csComposites = map (unRawSQL . ctName) ctypes
}

-- Advantages for using this source over default one:
-- - keeps connections already established before traffic comes in (reduce latency)
-- - disposes connections outside of request (reduce latency)
-- - limits the amount of connections at the same time
createPoolSource :: ConnectionSettings
                 -> IO (ConnectionTracker -> ConnectionSource)
createPoolSource cs = do
  pool <- createPool (connect cs) disconnect
    1  -- number of subpools, we do not need that functionality
    10 -- connection linger time after returned to pool
    maxConnections
  return $ \tracker -> ConnectionSource {
    withConnection = withResource' tracker pool . (clearStats >=>)
  }
  where
    withResource' :: (MonadBase IO m, MonadMask m)
                  => ConnectionTracker
                  -> Pool Connection
                  -> (Connection -> m a)
                  -> m a
    withResource' (ConnectionTracker tracker) pool m = mask $ \restore -> do
      (resource, local) <- liftBase $ takeResource pool
      (allocatedNow, availableNow) <- internalPoolState local
      tracker allocatedNow availableNow
      ret <- restore (m resource) `onException` do
        liftBase (destroyResource pool local resource)
      liftBase $ putResource local resource
      return ret

    internalPoolState local = liftBase $ (,)
      <$> readTVarIO (inUse local)
      <*> (length <$> readTVarIO (entries local))

    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ (\cd -> cd { cdStats = ConnectionStats 0 0 0 0 }) <$> mconn
      return conn

withPostgreSQL :: (MonadBase IO m, MonadMask m)
               => ConnectionSource -> DBT m a -> m a
withPostgreSQL cs = runDBT cs def
