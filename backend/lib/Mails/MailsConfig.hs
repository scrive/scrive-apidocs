{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.MailsConfig
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Mails.MailsConfig
    ( MailsConfig(..)
    , defaultMailsConfig
    ) where

import Data.Unjson

import KontraPrelude

-- | Configuration of mails
data MailsConfig = MailsConfig {
    isBackdoorOpen       :: Bool
  } deriving (Eq, Ord, Show)

unjsonMailsConfig :: UnjsonDef MailsConfig
unjsonMailsConfig = objectOf $ pure MailsConfig
  <*> field "backdoor_open"
      isBackdoorOpen
      "Should backdoor be open"
instance Unjson MailsConfig where
  unjsonDef = unjsonMailsConfig

defaultMailsConfig :: MailsConfig
defaultMailsConfig = MailsConfig {
    isBackdoorOpen        = False
}
