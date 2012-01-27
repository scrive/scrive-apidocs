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

-- | Configuration of mails
data MailsConfig = MailsConfig {
    isBackdoorOpen       :: Bool
  , ourInfoEmail         :: String
  , ourInfoEmailNiceName :: String
  } deriving (Read, Eq, Ord, Show)

defaultMailsConfig :: MailsConfig
defaultMailsConfig = MailsConfig {
    isBackdoorOpen        = False
  , ourInfoEmail          = "development-system@skrivapa.se"
  , ourInfoEmailNiceName  = "Development"
}
