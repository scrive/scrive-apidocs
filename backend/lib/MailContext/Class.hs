{-# LANGUAGE TemplateHaskell #-}
module MailContext.Class (
    I.MailContext
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans

import qualified MailContext.Internal as I

class Monad m => MailContextMonad m where
  getMailContext :: m I.MailContext

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-} (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
  getMailContext = lift getMailContext
