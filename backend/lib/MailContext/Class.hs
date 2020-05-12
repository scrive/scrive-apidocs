module MailContext.Class
  ( MailContext(MailContext)
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans

import MailContext.Internal

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-} (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
  getMailContext = lift getMailContext
