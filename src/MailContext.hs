{-# LANGUAGE OverlappingInstances #-}
-- | A subset of 'Context' needed for e.g. generating mail and SMS messages
module MailContext
  ( MailContext(..)
  , MailContextMonad(..)
  , MailContextT(..)
  , runMailContextT
  ) where

import BrandedDomain.BrandedDomain (BrandedDomain)
import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, defaultLiftWith, defaultRestoreT)
import IPAddress (IPAddress)
import Mails.MailsConfig (MailsConfig)
import MinutesTime (UTCTime)
import User.Model (User, Lang)

data MailContext = MailContext
  { mctxhostpart :: String
  , mctxmailsconfig :: MailsConfig
  , mctxlang :: Lang
  , mctxcurrentBrandedDomain :: Maybe BrandedDomain
  , mctxipnumber :: IPAddress
  , mctxtime :: UTCTime
  , mctxmaybeuser :: Maybe User
  }
  deriving Show

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

newtype MailContextT m a = MailContextT { unMailContextT :: ReaderT MailContext m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b)

runMailContextT :: MailContext -> MailContextT m a -> m a
runMailContextT ts m = runReaderT (unMailContextT m) ts

instance Monad m => MailContextMonad (MailContextT m) where
  getMailContext = MailContextT ask

instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
    getMailContext = lift getMailContext

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
