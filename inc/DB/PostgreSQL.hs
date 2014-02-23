module DB.PostgreSQL
  ( withPostgreSQL
  , createPostgreSQLConnectionPool
  , withPostgreSQLConnectionPool
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.HDBC
import qualified Control.Exception.Lifted as E
import qualified Database.HDBC.PostgreSQL as PG

import DB.Core
import DB.Nexus
import DB.SQL
import Data.Monoid
import Data.Pool

withPostgreSQL :: (MonadBaseControl IO m, MonadIO m) => String -> DBT m a -> m a
withPostgreSQL conf m =
  E.bracket (liftIO $ PG.connectPostgreSQL conf) (liftIO . disconnect) $ \conn -> do
    nex <- mkNexus conn
    res <- runDBT nex (DBEnvSt Nothing [] Nothing (mempty :: SQL)) m
    liftIO $ commit conn
    return res


createPostgreSQLConnectionPool :: String -> IO (Pool PG.Connection)
createPostgreSQLConnectionPool dbconnstring = do
  createPool (PG.connectPostgreSQL dbconnstring) -- connect
             disconnect                          -- disconnect
             1                                   -- number of subpools, we do not need that functionality
             10                                  -- connection linger time after returned to pool
             50                                  -- high water mark

-- Advantages for using this function over direct connections to PostgreSQL:
-- - keeps connections already established before traffic comes in (reduce latency)
-- - disposes connections outside of request (reduce latency)
-- - limits the amount of connections at the same time
withPostgreSQLConnectionPool :: (MonadBaseControl IO m, MonadIO m) => Pool PG.Connection -> DBT m a -> m a
withPostgreSQLConnectionPool pool m =
  E.bracket (liftIO (takeResource pool)) (\(conn,localPool) -> (liftIO (rollback conn >> (putResource localPool conn)))) $ \(conn,_localPool) -> do
    nex <- mkNexus conn
    res <- runDBT nex (DBEnvSt Nothing [] Nothing (mempty :: SQL)) m
    liftIO $ commit conn
    return res
