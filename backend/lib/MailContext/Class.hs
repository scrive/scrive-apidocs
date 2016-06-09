{-# LANGUAGE OverlappingInstances #-}
module MailContext.Class (
    MailContext(..)
  , mctxDomainUrl
  , MailContextMonad(..)
  ) where

import Control.Monad.Trans
import Data.Time

import BrandedDomain.BrandedDomain
import KontraPrelude
import User.Model

data MailContext = MailContext {
  mctxlang                 :: !Lang
, mctxcurrentBrandedDomain :: !BrandedDomain
, mctxtime                 :: !UTCTime
} deriving Show

mctxDomainUrl :: MailContext -> String
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
