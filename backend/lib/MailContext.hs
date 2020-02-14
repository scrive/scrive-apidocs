-- | A subset of 'Context' needed for e.g. generating mail and SMS messages
module MailContext (
    module MailContext.Class
  , MailContextMonad(..)
  , MailContextT(..)
  , runMailContextT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control

import MailContext.Class

newtype MailContextT m a = MailContextT { unMailContextT :: ReaderT MailContext m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

runMailContextT :: MailContext -> MailContextT m a -> m a
runMailContextT ts m = runReaderT (unMailContextT m) ts

instance Monad m => MailContextMonad (MailContextT m) where
  getMailContext = MailContextT ask

instance MonadBaseControl IO m => MonadBaseControl IO (MailContextT m) where
  type StM (MailContextT m) a = ComposeSt MailContextT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl MailContextT where
  type StT MailContextT m = StT (ReaderT MailContext) m
  liftWith = defaultLiftWith MailContextT unMailContextT
  restoreT = defaultRestoreT MailContextT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
