{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.MailsConfig
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Basic email sending configuration with SendGrid. Conf file in main dir with name mail.conf
-- We use Show/Read instances for parsing. We may have some problems with unicode on win machines,
-- but don't care about this for now
-----------------------------------------------------------------------------
module Mails.MailsConfig
    ( MailsConfig(..)
    , defaultMailsConfig
    ) where

import Data.Unjson
import Control.Applicative

-- | Configuration of mails
data MailsConfig = MailsConfig {
    isBackdoorOpen       :: Bool
  , ourInfoEmail         :: String
  , ourInfoEmailNiceName :: String
  } deriving (Read, Eq, Ord, Show)

unjsonMailsConfig :: UnjsonDef MailsConfig
unjsonMailsConfig = objectOf $ pure MailsConfig
  <*> fieldDef "backdoor_open" False
      isBackdoorOpen
      "Should backdoor be open"
  <*> field "info_email"
      ourInfoEmail
      "Info email"
  <*> field "info_email_nice_name"
      ourInfoEmailNiceName
      "Info email nice name"

instance Unjson MailsConfig where
  unjsonDef = unjsonMailsConfig

defaultMailsConfig :: MailsConfig
defaultMailsConfig = MailsConfig {
    isBackdoorOpen        = False
  , ourInfoEmail          = "development-system@skrivapa.se"
  , ourInfoEmailNiceName  = "Development"
}
