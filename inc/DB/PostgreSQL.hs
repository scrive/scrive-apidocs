module DB.PostgreSQL where

import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes
import qualified Data.ByteString as BS

pgConnSettings :: BS.ByteString -> ConnectionSettings
pgConnSettings dbconf = defaultSettings { csConnInfo = dbconf }

-- Advantages for using this source over default one:
-- - keeps connections already established before traffic comes in (reduce latency)
-- - disposes connections outside of request (reduce latency)
-- - limits the amount of connections at the same time
createPoolSource :: ConnectionSettings -> IO ConnectionSource
createPoolSource cs = poolSource cs
  1  -- number of subpools, we do not need that functionality
  10 -- connection linger time after returned to pool
  50 -- high water mark

withPostgreSQL :: MonadBaseControl IO m => ConnectionSource -> DBT m a -> m a
withPostgreSQL cs = runDBT cs defaultTransactionSettings
