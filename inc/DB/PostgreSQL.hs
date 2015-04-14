module DB.PostgreSQL where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Data.Monoid.Utils
import Data.Pool
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection
import System.Random
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
    15 -- high water mark
  return ConnectionSource {
    withConnection = withResource' pool . (clearStats >=>)
  }
  where
    withResource' :: (MonadBase IO m, MonadMask m)
                  => Pool Connection -> (Connection -> m a) -> m a
    withResource' pool m = mask $ \restore -> do
      uid <- show <$> liftBase (randomRIO (100000, 999999::Int32))
      logInfo uid "acquiring connection from the pool..."
      (resource, local) <- liftBase $ takeResource pool
      (allocatedNow, availableNow) <- internalPoolState local
      logInfo uid $ "connection acquired (" ++ show allocatedNow <+> "allocated," <+> show availableNow <+> "available)"
      ret <- restore (m resource) `onException` do
        logInfo uid "exception thrown while executing action, destroying connection"
        liftBase (destroyResource pool local resource)
        (allocatedAfter, availableAfter) <- internalPoolState local
        logInfo uid $ "connection destroyed (" ++ show allocatedAfter <+> "allocated," <+> show availableAfter <+> "available)"
      logInfo uid "returning connection to the pool..."
      liftBase $ putResource local resource
      (allocatedAfter, availableAfter) <- internalPoolState local
      logInfo uid $ "connection returned (" ++ show allocatedAfter <+> "allocated," <+> show availableAfter <+> "available)"
      return ret

    logInfo :: MonadBase IO m => String -> String -> m ()
    logInfo uid = logger . (("withResource (" ++ uid ++ "): ") <+>)

    internalPoolState local = liftBase . atomically $ (,)
      <$> readTVar (inUse local)
      <*> (length <$> readTVar (entries local))

    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ (\cd -> cd { cdStats = ConnectionStats 0 0 0 0 }) <$> mconn
      return conn

withPostgreSQL :: (MonadBase IO m, MonadMask m)
               => ConnectionSource -> DBT m a -> m a
withPostgreSQL cs = runDBT cs def
