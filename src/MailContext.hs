-- | A subset of 'Context' needed for e.g. generating mail and SMS messages
module MailContext (
    module MailContext.Class
  , MailContext(..)
  , MailContextMonad(..)
  , MailContextT(..)
  , runMailContextT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import MailContext.Class

newtype MailContextT m a = MailContextT { unMailContextT :: ReaderT MailContext m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b)

runMailContextT :: MailContext -> MailContextT m a -> m a
runMailContextT ts m = runReaderT (unMailContextT m) ts

instance Monad m => MailContextMonad (MailContextT m) where
  getMailContext = MailContextT ask

instance MonadBaseControl IO m => MonadBaseControl IO (MailContextT m) where
  newtype StM (MailContextT m) a = StM { unStM :: ComposeSt MailContextT m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl MailContextT where
  newtype StT MailContextT m = StT { unStT :: StT (ReaderT MailContext) m }
  liftWith = defaultLiftWith MailContextT unMailContextT StT
  restoreT = defaultRestoreT MailContextT unStT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
