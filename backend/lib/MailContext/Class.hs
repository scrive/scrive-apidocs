{-# LANGUAGE TemplateHaskell #-}
module MailContext.Class (
    MailContext
  , mctxDomainUrl
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans
import Optics

import MailContext.Internal

mctxDomainUrl :: Lens' MailContext Text
mctxDomainUrl = #mctxCurrentBrandedDomain % #bdUrl

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

-- | Generic, overlapping instance.
instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
  getMailContext = lift getMailContext
