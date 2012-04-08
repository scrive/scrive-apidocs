{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module DB.Core where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Happstack.Server
import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW

import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB.Nexus

data DBEnv = DBEnv {
    envRNG   :: CryptoRNGState
  , envNexus :: Nexus
  }

newtype DBT m a = DBT { unDBT :: StateT DBEnv m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

runDBT :: Monad m => DBEnv -> DBT m a -> m (a, DBEnv)
runDBT env f = runStateT (unDBT f) env

instance Monad m => CryptoRNG (DBT m) where
  getCryptoRNGState = DBT $ gets envRNG

instance MonadBase b m => MonadBase b (DBT m) where
  liftBase = liftBaseDefault

instance MonadTransControl DBT where
  newtype StT DBT a = StDBT { unStDBT :: StT (StateT DBEnv) a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT

instance MonadError e m => MonadError e (DBT m) where
  throwError     = lift . throwError
  catchError m h = DBT $ catchError (unDBT m) (unDBT . h)

instance MonadIO m => MonadIO (DBT m) where
  liftIO = lift . liftIO

instance MonadPlus m => MonadPlus (DBT m) where
  mzero     = lift mzero
  mplus a b = DBT $ mplus (unDBT a) (unDBT b)

instance MonadReader r m => MonadReader r (DBT m) where
  ask     = lift ask
  local f = DBT . local f . unDBT

instance MonadState s m => MonadState s (DBT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (DBT m) where
  tell   = lift . tell
  listen = DBT . listen . unDBT
  pass   = DBT . pass . unDBT

-- Happstack specific instances, to be moved somewhere else

instance FilterMonad f m => FilterMonad f (DBT m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter m   = DBT . StateT $ \s -> reorder `liftM` getFilter (runDBT s m)
    where reorder ((a, s), f) = ((a, f), s)

instance (HasRqData m, Monad m) => HasRqData (DBT m) where
  askRqEnv       = lift askRqEnv
  localRqEnv f m = DBT . StateT $ \s -> localRqEnv f (runDBT s m)
  rqDataError    = lift . rqDataError

instance ServerMonad m => ServerMonad (DBT m) where
  askRq       = lift askRq
  localRq f m = DBT . StateT $ \s -> localRq f (runDBT s m)

instance WebMonad r m => WebMonad r (DBT m) where
  finishWith = lift . finishWith

-- Class for accessing DBEnv object

class (CryptoRNG m, MonadIO m) => MonadDB m where
  getDBEnv :: m DBEnv

instance MonadIO m => MonadDB (DBT m) where
  getDBEnv = DBT get

instance (CryptoRNG (ReaderT r m), MonadDB m) => MonadDB (ReaderT r m) where
  getDBEnv = lift getDBEnv

{-instance MonadDB m => MonadDB (ContT r m) where
  getDBEnv = lift getDBEnv

instance (Error e, MonadDB m) => MonadDB (ErrorT e m) where
  getDBEnv = lift getDBEnv

instance MonadDB m => MonadDB (IdentityT m) where
  getDBEnv = lift getDBEnv

instance MonadDB m => MonadDB (ListT m) where
  getDBEnv = lift getDBEnv

instance MonadDB m => MonadDB (MaybeT m) where
  getDBEnv = lift getDBEnv

instance MonadDB m => MonadDB (SS.StateT s m) where
  getDBEnv = lift getDBEnv

instance MonadDB m => MonadDB (LS.StateT s m) where
  getDBEnv = lift getDBEnv

instance (MonadDB m, Monoid w) => MonadDB (LW.WriterT w m) where
  getDBEnv = lift getDBEnv

instance (MonadDB m, Monoid w) => MonadDB (SW.WriterT w m) where
  getDBEnv = lift getDBEnv
  -}
