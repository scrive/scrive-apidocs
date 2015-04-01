module JobQueue.Consumer (
    ConsumerID
  , registerConsumer
  , unregisterConsumer
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Database.PostgreSQL.PQTypes

import JobQueue.Config
import KontraPrelude

-- | ID of a consumer.
newtype ConsumerID = ConsumerID Int64
  deriving (Eq, Ord, PQFormat)
instance FromSQL ConsumerID where
  type PQBase ConsumerID = PQBase Int64
  fromSQL mbase = ConsumerID <$> fromSQL mbase
instance ToSQL ConsumerID where
  type PQDest ConsumerID = PQDest Int64
  toSQL (ConsumerID n) = toSQL n

instance Show ConsumerID where
  showsPrec p (ConsumerID n) = showsPrec p n

-- | Register consumer in the consumers table,
-- so that it can reserve jobs using acquired ID.
registerConsumer
  :: (MonadBase IO m, MonadMask m)
  => ConsumerConfig n idx job
  -> ConnectionSource
  -> m ConsumerID
registerConsumer ConsumerConfig{..} cs = runDBT cs ts $ do
  runSQL_ $ smconcat [
      "INSERT INTO" <+> raw ccConsumersTable
    , "(name, last_activity) VALUES (" <?> unRawSQL ccJobsTable <> ", now())"
    , "RETURNING id"
    ]
  fetchOne runIdentity

-- | Unregister consumer with a given ID.
unregisterConsumer
  :: (MonadBase IO m, MonadMask m)
  => ConsumerConfig n idx job
  -> ConnectionSource
  -> ConsumerID
  -> m ()
unregisterConsumer ConsumerConfig{..} cs wid = runDBT cs ts $ do
  runSQL_ $ smconcat [
      "DELETE FROM " <+> raw ccConsumersTable
    , "WHERE id =" <?> wid
    , "  AND name =" <?> unRawSQL ccJobsTable
    ]

----------------------------------------

ts :: TransactionSettings
ts = def {
  tsAutoTransaction = False
, tsIsolationLevel = ReadCommitted
, tsPermissions = ReadWrite
}
