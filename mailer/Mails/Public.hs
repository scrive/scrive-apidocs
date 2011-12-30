{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mails.Public where

import Control.Applicative
import Database.HDBC
import Data.Data
import Data.Int
import Data.Either
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

import DB.Classes
import DB.Derive
import DB.Utils
import OurPrelude

newtype MailID = MailID { fromMailID :: Int64 }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''MailID)
$(newtypeDeriveConvertible ''MailID)

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

data CreateEmail = CreateEmail Address [Address]
instance DBUpdate CreateEmail MailID where
  dbUpdate (CreateEmail sender to) = $(fromJust)
    <$> getOne "INSERT INTO mails (sender, receivers) VALUES (?, ?) RETURNING id"
      [toSql sender, toSql to]

data AddContentToEmail = AddContentToEmail MailID String String [Attachment] String
instance DBUpdate AddContentToEmail Bool where
  dbUpdate (AddContentToEmail mid title content attachments xsmtpapi) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE mails SET"
      ++ "  title = ?"
      ++ ", content = ?"
      ++ ", attachments = ?"
      ++ ", x_smtpapi = ? WHERE id = ?") [
          toSql title
        , toSql content
        , toSql attachments
        , toSql xsmtpapi
        , toSql mid
        ]
    oneRowAffectedGuard r
