module MailContext.Class (
    MailContext
  , module MailContext.Labels
  , mctxDomainUrl
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans

import BrandedDomain.BrandedDomain
import KontraPrelude
import MailContext.Internal
import MailContext.Labels

mctxDomainUrl :: MailContext -> String
mctxDomainUrl = get (bdUrl . mctxcurrentBrandedDomain)

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

-- | Generic, overlapping instance.
instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
    getMailContext = lift getMailContext
