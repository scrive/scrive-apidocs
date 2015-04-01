module DB.RowCache
  ( ID
  , HasID(..)
  , GetRow(..)
  , RowCacheT(..)
  , rowCache
  , rowCacheID
  , updateRow
  , updateRowWithID
  , runRowCacheTID
  , runRowCacheT
  , runRowCacheTM
  )  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (runReaderT, ask, ReaderT)
import Control.Monad.State (MonadTrans, get, put, lift, evalStateT, StateT, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM)
import Database.PostgreSQL.PQTypes (MonadDB)

import KontraPrelude

-- | Return an identifier type for a row 'r' that can be used to retrieve rows from storage
type family ID r

-- | TODO move this to separate module and use it for e.g. signatory link ids etc to get more general functions that work on data with identifiers
class HasID r where
  -- | Retrieve the identifier for a row
  getID :: r -> ID r

class Monad m => GetRow r m  where
  -- | Get a row from storage given its identifier
  getRow :: ID r -> m r

-- | Monad transformer for maintaining a cached row value or an invalid
-- mark, and remembering the row's identifier
newtype RowCacheT r m a = RowCacheT { unRowCacheT :: InnerRowCacheT r m a }
  deriving (Applicative, Functor, Monad, MonadDB, MonadIO, MonadBase b)

-- | Fetch the row and perform an operation that updates the stored row (and therefore marks the cached row as invalid)
updateRow :: GetRow r m => (r -> RowCacheT r m a) -> RowCacheT r m a
updateRow = updateRow' rowCache

-- | Retrieve the row identifier and perform an operation that updates the stored row (and therefore marks the cached row as invalid)
updateRowWithID :: Monad m => (ID r -> RowCacheT r m a) -> RowCacheT r m a
updateRowWithID = updateRow' rowCacheID

-- | Run a RowCache computation given a row identifier
runRowCacheTID :: (Monad m, HasID r) => ID r -> RowCacheT r m a -> m a
runRowCacheTID i (RowCacheT m) = flip runReaderT i $ flip evalStateT Invalid m

-- | Run a RowCache computation given a row
runRowCacheT :: (Monad m, HasID r) => r -> RowCacheT r m a -> m a
runRowCacheT r (RowCacheT m) = flip runReaderT (getID r) $ flip evalStateT (Row r) m

-- | Run a RowCache computation given a computation that obtains a row
runRowCacheTM :: (Monad m, HasID r) => m r -> RowCacheT r m a -> m a
runRowCacheTM mr m = mr >>= \r -> runRowCacheT r m

data RowState r =
   Row r
 | Invalid
 deriving Show

type InnerRowCacheT r m = StateT (RowState r) (ReaderT (ID r) m)

-- | Retrieve a row from the cache, or from the storage if the cache is invalid
rowCache :: GetRow r m => RowCacheT r m r
rowCache = RowCacheT get >>= \case
                        Row r -> return r
                        Invalid -> do
                          r <- rowCacheID >>= lift . getRow
                          RowCacheT $ put (Row r)
                          return r

-- | Return the row's ID
rowCacheID :: Monad m => RowCacheT r m (ID r)
rowCacheID = RowCacheT ask

-- | Mark the cache as invalid
setRowInvalid :: Monad m => RowCacheT r m ()
setRowInvalid = RowCacheT $ put Invalid

-- | Helper function for update operations that should mark the cache as invalid
updateRow' :: Monad m => RowCacheT r m b -> (b -> RowCacheT r m a) -> RowCacheT r m a
updateRow' cache m = do
  a <- cache >>= m
  setRowInvalid
  return a

-- Instances

instance MonadTrans (RowCacheT r) where
  lift = RowCacheT . lift . lift

instance MonadBaseControl b m => MonadBaseControl b (RowCacheT r m) where
  newtype StM (RowCacheT r m) a = StM { unStM :: ComposeSt (RowCacheT r) m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl (RowCacheT r) where
  newtype StT (RowCacheT r) a = StT { unStT :: StT (ReaderT (ID r)) (StT (StateT (RowState r)) a) }
  liftWith = \f -> RowCacheT $ liftWith $ \run ->
                               liftWith $ \run' -> f $ liftM StT . run' . run . unRowCacheT
  restoreT = RowCacheT . restoreT . restoreT . liftM unStT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
