module DB.Core where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
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

type InnerDBT = ReaderT Nexus

newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadPlus, MonadTrans)

mapDBT :: (m a -> n b) -> DBT m a -> DBT n b
mapDBT f m = withNexus $ \nex -> f (runDBT nex m)

runDBT :: Nexus -> DBT m a -> m a
runDBT env f = runReaderT (unDBT f) env

withNexus :: (Nexus -> m a) -> DBT m a
withNexus = DBT . ReaderT

instance MonadBase b m => MonadBase b (DBT m) where
  liftBase = liftBaseDefault

instance MonadTransControl DBT where
  newtype StT DBT a = StDBT { unStDBT :: StT InnerDBT a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT

instance MonadError e m => MonadError e (DBT m) where
  throwError     = lift . throwError
  catchError m h = withNexus $ \nex -> catchError (runDBT nex m) (runDBT nex . h)

instance MonadReader r m => MonadReader r (DBT m) where
  ask     = lift ask
  local f = mapDBT $ local f

instance MonadState s m => MonadState s (DBT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (DBT m) where
  tell   = lift . tell
  listen = mapDBT listen
  pass   = mapDBT pass

-- Happstack specific instances, to be moved somewhere else

instance FilterMonad f m => FilterMonad f (DBT m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter     = mapDBT getFilter

instance (HasRqData m, Monad m) => HasRqData (DBT m) where
  askRqEnv     = lift askRqEnv
  localRqEnv f = mapDBT $ localRqEnv f
  rqDataError  = lift . rqDataError

instance ServerMonad m => ServerMonad (DBT m) where
  askRq     = lift askRq
  localRq f = mapDBT $ localRq f

instance WebMonad r m => WebMonad r (DBT m) where
  finishWith = lift . finishWith

-- Class for accessing DBEnv object

class MonadIO m => MonadDB m where
  getNexus   :: m Nexus
  localNexus :: (Nexus -> Nexus) -> m a -> m a

instance MonadIO m => MonadDB (DBT m) where
  getNexus     = DBT ask
  localNexus f = DBT . local f . unDBT

instance MonadDB m => MonadDB (CryptoRNGT m) where
  getNexus     = lift getNexus
  localNexus f = mapCryptoRNGT $ localNexus f

instance MonadDB m => MonadDB (ReaderT r m) where
  getNexus     = lift getNexus
  localNexus f = mapReaderT $ localNexus f

instance MonadDB m => MonadDB (ContT r m) where
  getNexus     = lift getNexus
  localNexus f = mapContT $ localNexus f

instance (Error e, MonadDB m) => MonadDB (ErrorT e m) where
  getNexus     = lift getNexus
  localNexus f = mapErrorT $ localNexus f

instance MonadDB m => MonadDB (IdentityT m) where
  getNexus     = lift getNexus
  localNexus f = mapIdentityT $ localNexus f

instance MonadDB m => MonadDB (ListT m) where
  getNexus     = lift getNexus
  localNexus f = mapListT $ localNexus f

instance MonadDB m => MonadDB (MaybeT m) where
  getNexus     = lift getNexus
  localNexus f = mapMaybeT $ localNexus f

instance MonadDB m => MonadDB (SS.StateT s m) where
  getNexus     = lift getNexus
  localNexus f = SS.mapStateT $ localNexus f

instance MonadDB m => MonadDB (LS.StateT s m) where
  getNexus     = lift getNexus
  localNexus f = LS.mapStateT $ localNexus f

instance (MonadDB m, Monoid w) => MonadDB (LW.WriterT w m) where
  getNexus     = lift getNexus
  localNexus f = LW.mapWriterT $ localNexus f

instance (MonadDB m, Monoid w) => MonadDB (SW.WriterT w m) where
  getNexus     = lift getNexus
  localNexus f = SW.mapWriterT $ localNexus f
