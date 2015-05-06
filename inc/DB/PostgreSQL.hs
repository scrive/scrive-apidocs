module DB.PostgreSQL where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Pool
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection
import qualified Data.ByteString as BS

import DB.Model.CompositeType
import KontraPrelude

pgConnSettings :: BS.ByteString -> [CompositeType] -> ConnectionSettings
pgConnSettings dbconf ctypes = def {
  csConnInfo = dbconf
, csComposites = map (unRawSQL . ctName) ctypes
}

-- Advantages for using this source over default one:
-- - keeps connections already established before traffic comes in (reduce latency)
-- - disposes connections outside of request (reduce latency)
-- - limits the amount of connections at the same time
createPoolSource :: (forall m. MonadBase IO m => String -> m ())
                 -> ConnectionSettings
                 -> IO ConnectionSource
createPoolSource logger cs = do
  pool <- createPool (connect cs) disconnect
    1  -- number of subpools, we do not need that functionality
    10 -- connection linger time after returned to pool
    maxConnections
  return ConnectionSource {
    withConnection = withResource' pool . (clearStats >=>)
  }
  where
    maxConnections :: Int
    maxConnections = 15

    withResource' :: (MonadBase IO m, MonadMask m)
                  => Pool Connection -> (Connection -> m a) -> m a
    withResource' pool m = mask $ \restore -> do
      (resource, local) <- liftBase $ takeResource pool
      (allocatedNow, availableNow) <- internalPoolState local
      when (allocatedNow == maxConnections && availableNow == 0) $ do
        logger $ "withResource: limit of available connections reached"
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
