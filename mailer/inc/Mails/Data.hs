{-# LANGUAGE TemplateHaskell #-}
module Mails.Data (
    MailID
  , EventID
  , XSMTPAttrs(..)
  , Attachment(..)
  , Address(..)
  , SendGridEvent(..)
  , MailGunEvent(..)
  , Event(..)
  , Mail(..)
  ) where

import Data.Data
import Data.Int
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import DB.Derive
import MagicHash (MagicHash)
import File.FileID

newtype MailID = MailID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''MailID)
$(newtypeDeriveConvertible ''MailID)

newtype EventID = EventID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''EventID)
$(newtypeDeriveConvertible ''EventID)

newtype XSMTPAttrs = XSMTPAttrs { fromXSMTPAttrs :: [(String, String)] }
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| XSMTPAttrs |])

instance Monoid XSMTPAttrs where
  mempty = XSMTPAttrs []
  (XSMTPAttrs a) `mappend` (XSMTPAttrs b) = XSMTPAttrs (a ++ b)

data Attachment = Attachment {
    attName    :: String
  , attContent :: Either BS.ByteString FileID
  } deriving (Eq, Ord, Show, Typeable)

data Address = Address {
    addrName  :: String
  , addrEmail :: String
  } deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| Address |])
$(jsonableDeriveConvertible [t| [Address] |])

data SendGridEvent =
    SG_Processed
  | SG_Opened
  | SG_Dropped String              -- ^ drop reason
  | SG_Deferred String Int32       -- ^ response, delivery attempt
  | SG_Delivered String            -- ^ response from mta
  | SG_Bounce String String String -- ^ status, reason, type
  | SG_SpamReport
  | SG_Unsubscribe
    deriving (Eq, Ord, Show, Data, Typeable)

data MailGunEvent =
    MG_Opened
  | MG_Delivered
  | MG_Clicked String               -- ^ url
  | MG_Unsubscribed String          -- ^ domain
  | MG_Complained String            -- ^ domain
  | MG_Bounced String String String -- ^ domain, code, error
  | MG_Dropped String               -- ^ drop reason
    deriving (Eq, Ord, Show, Data, Typeable)

data Event =
    SendGridEvent String SendGridEvent String -- ^ email, event, category
  | MailGunEvent String MailGunEvent          -- ^ email, event
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| Event |])

data Mail = Mail {
    mailID          :: MailID
  , mailToken       :: MagicHash
  , mailFrom        :: Address
  , mailTo          :: [Address]
  , mailTitle       :: String
  , mailContent     :: String
  , mailAttachments :: [Attachment]
  , mailXSMTPAttrs  :: XSMTPAttrs
  , mailServiceTest :: Bool
  } deriving (Eq, Ord, Show)
