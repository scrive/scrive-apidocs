-- | This module provides simple monad transformer for carrying
-- acidic state around and overrides update/query functions from
-- Data.Acid so they don't require passing state as parameter.
module Acid.Monad where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Acid hiding (update, query)
import Data.Acid.Advanced
import Happstack.Server
import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW

import Control.Monad.Trans.Control.Util

-- | Monad transformer with acidic state.
newtype AcidT st m a = AcidT { unAcidT :: ReaderT st m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadPlus, MonadTrans)

mapAcidT :: (m a -> n b) -> AcidT st m a -> AcidT st n b
mapAcidT f m = withAcidStore $ \s -> f (runAcidT s m)

runAcidT :: st -> AcidT st m a -> m a
runAcidT st m = runReaderT (unAcidT m) st

withAcidStore :: (st -> m a) -> AcidT st m a
withAcidStore = AcidT . ReaderT

instance MonadTransControl (AcidT st) where
  newtype StT (AcidT st) a = StAcidT { unStAcidT :: StT (ReaderT st) a }
  liftWith = defaultLiftWith AcidT unAcidT StAcidT
  restoreT = defaultRestoreT AcidT unStAcidT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (AcidT st m) where
  newtype StM (AcidT st m) a = StMAcidT { unStMAcidT :: ComposeSt (AcidT st) m a }
  liftBaseWith = defaultLiftBaseWith StMAcidT
  restoreM     = defaultRestoreM unStMAcidT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadError e m => MonadError e (AcidT st m) where
  throwError     = lift . throwError
  catchError m h = withAcidStore $ \st -> catchError (runAcidT st m) (runAcidT st . h)

instance MonadReader r m => MonadReader r (AcidT st m) where
  ask     = lift ask
  local f = mapAcidT $ local f

instance MonadState s m => MonadState s (AcidT st m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (AcidT st m) where
  tell   = lift . tell
  listen = mapAcidT listen
  pass   = mapAcidT pass

-- Happstack specific instances, to be moved somewhere else

instance FilterMonad f m => FilterMonad f (AcidT st m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter     = mapAcidT getFilter

instance (HasRqData m, Monad m) => HasRqData (AcidT st m) where
  askRqEnv     = lift askRqEnv
  localRqEnv f = mapAcidT $ localRqEnv f
  rqDataError  = lift . rqDataError

instance ServerMonad m => ServerMonad (AcidT st m) where
  askRq     = lift askRq
  localRq f = mapAcidT $ localRq f

instance WebMonad r m => WebMonad r (AcidT st m) where
  finishWith = lift . finishWith

-- | Monads carrying around acid store.
class Monad m => AcidStore st m where
  getAcidStore :: m st

instance Monad m => AcidStore st (AcidT st m) where
  getAcidStore = AcidT ask

instance AcidStore st m => AcidStore st (ReaderT r m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (ContT r m) where
  getAcidStore = lift getAcidStore

instance (Error e, AcidStore st m) => AcidStore st (ErrorT e m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (IdentityT m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (ListT m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (MaybeT m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (SS.StateT s m) where
  getAcidStore = lift getAcidStore

instance AcidStore st m => AcidStore st (LS.StateT s m) where
  getAcidStore = lift getAcidStore

instance (AcidStore st m, Monoid w) => AcidStore st (LW.WriterT w m) where
  getAcidStore = lift getAcidStore

instance (AcidStore st m, Monoid w) => AcidStore st (SW.WriterT w m) where
  getAcidStore = lift getAcidStore

-- | Class indicating whether given monad contains given AcidState.
-- Note that this is different than AcidStore typeclass, because
-- HasAcidState gives a way to get whole acid store, which can have
-- multiple AcidState components or additional fields.
-- Example:
--
-- newtype T = T Int
-- newtype S = S String
-- data Store = Store { t :: AcidState T, s :: AcidState S }
-- type M = AcidT Store IO
--
-- Then you can get underlying store object in M monad by executing
-- getAcidStore, but to be able to get specific component (and make
-- query/update functions work), you need to define HasAcidState T M
-- and HasAcidState S M instances yourself. Alternatively, you could
-- write general instances:
--
-- instance AcidStore Store m => HasAcidState T m where
--   getAcidState = t `liftM` getAcidStore
-- instance AcidStore Store m => HasAcidState S m where
--   getAcidState = s `liftM` getAcidStore
--
class HasAcidState st m where
  getAcidState :: m (AcidState st)

query :: forall ev m. (MonadIO m, QueryEvent ev, HasAcidState (EventState ev) m) => ev -> m (EventResult ev)
query ev = do
  st <- getAcidState
  query' (st :: AcidState (EventState ev)) ev

update :: forall ev m. (MonadIO m, UpdateEvent ev, HasAcidState (EventState ev) m) => ev -> m (EventResult ev)
update ev = do
  st <- getAcidState
  update' (st :: AcidState (EventState ev)) ev
