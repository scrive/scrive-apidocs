module JobQueue.Consumer (
    ConsumerID
  , registerConsumer
  , unregisterConsumer
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

import JobQueue.Config

newtype ConsumerID = ConsumerID Int64
  deriving (Eq, Ord, PQFormat)
instance FromSQL ConsumerID where
  type PQBase ConsumerID = PQBase Int64
  fromSQL mbase = ConsumerID <$> fromSQL mbase
instance ToSQL ConsumerID where
  type PQDest ConsumerID = PQDest Int64
  toSQL (ConsumerID n) = toSQL n

instance Show ConsumerID where
  showsPrec p (ConsumerID n) = ("#" ++) . showsPrec p n

registerConsumer
  :: (MonadBase IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> m ConsumerID
registerConsumer cc cs = runDBT cs ts $ do
  runSQL_ $ smconcat [
      "INSERT INTO" <+> raw (ccConsumersTable cc)
    , "(last_activity) VALUES (now())"
    , "RETURNING id"
    ]
  fetchOne runIdentity

unregisterConsumer
  :: (MonadBase IO m, MonadMask m)
  => ConsumerConfig m idx job
  -> ConnectionSource
  -> ConsumerID
  -> m ()
unregisterConsumer cc cs wid = runDBT cs ts $ do
  runSQL_ $ smconcat [
      "DELETE FROM " <+> raw (ccConsumersTable cc)
    , "WHERE id =" <?> wid
    ]

----------------------------------------

ts :: TransactionSettings
ts = def {
  tsAutoTransaction = False
, tsIsolationLevel = ReadCommitted
, tsPermissions = ReadWrite
}
