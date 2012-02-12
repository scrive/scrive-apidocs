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
import Data.Either
import Data.Int
import Database.HDBC
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import DB.Derive
import MagicHash (MagicHash)

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

data Attachment = Attachment {
    attName    :: String
  , attContent :: BS.ByteString
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Convertible [Attachment] SqlValue where
  safeConvert = jsonToSqlValue . map (\a -> a { attContent = B64.encode $ attContent a })

instance Convertible SqlValue [Attachment] where
  safeConvert v = check . partitionEithers . map f =<< jsonFromSqlValue v
    where
      check ([], x) = Right x
      check ((e:_), _) = Left e
      f a = case B64.decode $ attContent a of
              Right ds -> Right a { attContent = ds }
              Left e   -> Left ConvertError {
                  convSourceValue = show $ attContent a
                , convSourceType = "Base64 encoded ByteString"
                , convDestType = "ByteString"
                , convErrorMessage = "Couldn't convert from base64: " ++ e
              }

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
  } deriving (Eq, Ord, Show)
