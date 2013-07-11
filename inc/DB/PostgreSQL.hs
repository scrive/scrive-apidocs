module DB.PostgreSQL (withPostgreSQL) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.HDBC
import qualified Control.Exception.Lifted as E
import qualified Database.HDBC.PostgreSQL as PG

import DB.Core
import DB.Nexus
import DB.SQL
import Data.Monoid

withPostgreSQL :: (MonadBaseControl IO m, MonadIO m) => String -> DBT m a -> m a
withPostgreSQL conf m =
  E.bracket (liftIO $ PG.connectPostgreSQL conf) (liftIO . disconnect) $ \conn -> do
    nex <- mkNexus conn
    res <- runDBT nex (DBEnvSt Nothing [] Nothing (mempty :: SQL)) m
    liftIO $ commit conn
    return res
