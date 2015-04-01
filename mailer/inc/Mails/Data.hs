module Mails.Data (
    SenderType(..)
  , JobType(..)
  , MailerJob(..)
  , MailID
  , EventID
  , XSMTPAttrs(..)
  , Attachment(..)
  , Address(..)
  , SendGridEvent(..)
  , MailGunEvent(..)
  , Event(..)
  , Mail(..)
  , unjsonAddress
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes

import DB.Derive
import File.FileID
import KontraPrelude
import MagicHash (MagicHash)
import Utils.List

data SenderType = MasterSender | SlaveSender
  deriving (Eq, Ord, Show)

----------------------------------------

data JobType
  = CleanOldEmails
  | PerformServiceTest
  | CollectServiceTestResult
  deriving (Eq, Ord, Show)

jobTypeMapper :: [(JobType, ByteString)]
jobTypeMapper = [
    (CleanOldEmails, "clean_old_emails")
  , (PerformServiceTest, "perform_service_test")
  , (CollectServiceTestResult, "collect_service_test_result")
  ]

instance PQFormat JobType where
  pqFormat _ = pqFormat (undefined::ByteString)

instance FromSQL JobType where
  type PQBase JobType = PQBase ByteString
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing -> throwM InvalidValue {
        ivValue = v
      , ivValidValues = Just $ map snd jobTypeMapper
      }

instance ToSQL JobType where
  type PQDest JobType = PQBase ByteString
  toSQL tt = toSQL . $fromJust $ tt `lookup` jobTypeMapper

data MailerJob = MailerJob {
  mjType      :: !JobType
, mjAttempts  :: !Int32
} deriving (Eq, Ord, Show)

----------------------------------------

newtype MailID = MailID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''MailID)

instance FromSQL MailID where
  type PQBase MailID = PQBase Int64
  fromSQL mbase = MailID <$> fromSQL mbase
instance ToSQL MailID where
  type PQDest MailID = PQDest Int64
  toSQL (MailID n) = toSQL n

data Address = Address {
  addrName  :: !String
, addrEmail :: !String
} deriving (Eq, Ord, Read, Show, Data, Typeable)

unjsonAddress :: UnjsonDef Address
unjsonAddress = objectOf $ pure Address
  <*> field "name"
     addrName
     "Name in email address"
  <*> field "email"
     addrEmail
     "Email address"

instance Unjson Address where
  unjsonDef = unjsonAddress

instance PQFormat Address where
  pqFormat = const $ pqFormat (undefined::String)
instance FromSQL Address where
  type PQBase Address = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Address where
  type PQDest Address = PQDest String
  toSQL = jsonToSQL

instance PQFormat [Address] where
  pqFormat = const $ pqFormat (undefined::String)
instance FromSQL [Address] where
  type PQBase [Address] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [Address] where
  type PQDest [Address] = PQDest String
  toSQL = jsonToSQL

data Attachment = Attachment {
  attName    :: !String
, attContent :: !(Either ByteString FileID)
} deriving (Eq, Ord, Show)

type instance CompositeRow Attachment = (String, Maybe (Binary ByteString), Maybe FileID)

instance PQFormat Attachment where
  pqFormat = const "%mail_attachment"

instance CompositeFromSQL Attachment where
  toComposite (name, mcontent, mfid) = Attachment {
    attName = name
  , attContent = case (mcontent, mfid) of
    (Just (Binary content), Nothing) -> Left content
    (Nothing, Just fid) -> Right fid
    _ -> $unexpectedError "impossible due to the check constraint"
  }

newtype XSMTPAttrs = XSMTPAttrs { fromXSMTPAttrs :: [(String, String)] }
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat XSMTPAttrs where
  pqFormat = const $ pqFormat (undefined::String)
instance FromSQL XSMTPAttrs where
  type PQBase XSMTPAttrs = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL XSMTPAttrs where
  type PQDest XSMTPAttrs = PQDest String
  toSQL = jsonToSQL

instance Monoid XSMTPAttrs where
  mempty = XSMTPAttrs []
  XSMTPAttrs a `mappend` XSMTPAttrs b = XSMTPAttrs (a ++ b)

data Mail = Mail {
  mailID          :: !MailID
, mailToken       :: !MagicHash
, mailFrom        :: !Address
, mailTo          :: ![Address]
, mailReplyTo     :: !(Maybe Address)
, mailTitle       :: !String
, mailContent     :: !String
, mailAttachments :: ![Attachment]
, mailXSMTPAttrs  :: !XSMTPAttrs
, mailServiceTest :: !Bool
, mailAttempts    :: !Int32
} deriving (Eq, Ord, Show)

----------------------------------------

newtype EventID = EventID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''EventID)

instance FromSQL EventID where
  type PQBase EventID = PQBase Int64
  fromSQL mbase = EventID <$> fromSQL mbase
instance ToSQL EventID where
  type PQDest EventID = PQDest Int64
  toSQL (EventID n) = toSQL n

data SendGridEvent
  = SG_Processed
  | SG_Opened
  | SG_Dropped String              -- ^ drop reason
  | SG_Deferred String Int32       -- ^ response, delivery attempt
  | SG_Delivered String            -- ^ response from mta
  | SG_Bounce String String String -- ^ status, reason, type
  | SG_SpamReport
  | SG_Unsubscribe
    deriving (Eq, Ord, Show, Data, Typeable)

data MailGunEvent
  = MG_Opened
  | MG_Delivered
  | MG_Clicked !String                  -- ^ url
  | MG_Unsubscribed !String             -- ^ domain
  | MG_Complained !String               -- ^ domain
  | MG_Bounced String !String !String   -- ^ domain, code, error
  | MG_Dropped !String                  -- ^ drop reason
    deriving (Eq, Ord, Show, Data, Typeable)

data Event
  = SendGridEvent !String !SendGridEvent !String   -- ^ email, event, category
  | MailGunEvent !String !MailGunEvent             -- ^ email, event
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat Event where
  pqFormat = const $ pqFormat (undefined::String)
instance FromSQL Event where
  type PQBase Event = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Event where
  type PQDest Event = PQDest String
  toSQL = jsonToSQL
