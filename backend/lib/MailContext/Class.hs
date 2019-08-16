module MailContext.Class (
    MailContext
  , module MailContext.Labels
  , mctxDomainUrl
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans
import Data.Label

import BrandedDomain.BrandedDomain
import MailContext.Internal
import MailContext.Labels

mctxDomainUrl :: MailContext :-> Text
mctxDomainUrl = bdUrl . mctxcurrentBrandedDomain

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

-- | Generic, overlapping instance.
instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
    getMailContext = lift getMailContext
