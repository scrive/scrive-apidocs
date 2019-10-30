{-# LANGUAGE TemplateHaskell #-}
module MailContext.Class (
    I.MailContext
  , mctxDomainUrl
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans
import Optics

import qualified MailContext.Internal as I

mctxDomainUrl :: Lens' I.MailContext Text
mctxDomainUrl = #brandedDomain % #url

class Monad m => MailContextMonad m where
  getMailContext :: m I.MailContext

-- | Generic, overlapping instance.
instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
  getMailContext = lift getMailContext
