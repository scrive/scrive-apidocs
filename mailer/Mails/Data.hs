{-# LANGUAGE TemplateHaskell #-}
module Mails.Data (
    MailID
  , XSMTPAttrs(..)
  , Attachment(..)
  , Address(..)
  , SendGridEvent(..)
  , Event(..)
  ) where

import Data.Data
import Data.Either
import Data.Int
import Database.HDBC
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import DB.Derive

newtype MailID = MailID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''MailID)
$(newtypeDeriveConvertible ''MailID)

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
    Processed
  | Opened
  | Dropped String              -- ^ drop reason
  | Deferred String Int32       -- ^ response, delivery attempt
  | Delivered String            -- ^ response from mta
  | Bounce String String String -- ^ status, reason, type
    deriving (Eq, Ord, Show, Data, Typeable)

data Event = SendGridEvent SendGridEvent
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| Event |])
