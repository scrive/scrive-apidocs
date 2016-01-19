{-# LANGUAGE OverlappingInstances #-}
module MailContext.Class where

import Control.Monad.Trans
import Data.Time

import BrandedDomain.BrandedDomain
import KontraPrelude
import Mails.MailsConfig
import User.Model

data MailContext = MailContext {
  mctxhostpart :: String
, mctxmailsconfig :: MailsConfig
, mctxlang :: Lang
, mctxcurrentBrandedDomain :: BrandedDomain
, mctxtime :: UTCTime
} deriving Show

class Monad m => MailContextMonad m where
  getMailContext :: m MailContext

-- | Generic, overlapping instance.
instance (
    MailContextMonad m
  , Monad (t m)
  , MonadTrans t
  ) => MailContextMonad (t m) where
    getMailContext = lift getMailContext
