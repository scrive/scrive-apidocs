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

import Control.Applicative
import Data.Data
import Data.Int
import Data.Monoid
import Database.PostgreSQL.PQTypes
import qualified Data.ByteString.Char8 as BS

import DB.Derive
import MagicHash (MagicHash)
import File.FileID

newtype MailID = MailID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''MailID)

instance FromSQL MailID where
  type PQBase MailID = PQBase Int64
  fromSQL mbase = MailID <$> fromSQL mbase
instance ToSQL MailID where
  type PQDest MailID = PQDest Int64
  toSQL (MailID n) = toSQL n

newtype EventID = EventID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''EventID)

instance FromSQL EventID where
  type PQBase EventID = PQBase Int64
  fromSQL mbase = EventID <$> fromSQL mbase
instance ToSQL EventID where
  type PQDest EventID = PQDest Int64
  toSQL (EventID n) = toSQL n

newtype XSMTPAttrs = XSMTPAttrs { fromXSMTPAttrs :: [(String, String)] }
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat XSMTPAttrs where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL XSMTPAttrs where
  type PQBase XSMTPAttrs = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL XSMTPAttrs where
  type PQDest XSMTPAttrs = PQDest String
  toSQL = jsonToSQL

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
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance PQFormat Address where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL Address where
  type PQBase Address = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Address where
  type PQDest Address = PQDest String
  toSQL = jsonToSQL

instance PQFormat [Address] where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL [Address] where
  type PQBase [Address] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [Address] where
  type PQDest [Address] = PQDest String
  toSQL = jsonToSQL

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

instance PQFormat Event where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL Event where
  type PQBase Event = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Event where
  type PQDest Event = PQDest String
  toSQL = jsonToSQL

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
  , mailAttempt     :: Int32
  , mailReplyTo     :: [Address]
  } deriving (Eq, Ord, Show)
