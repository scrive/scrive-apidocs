module Mails.Types (
    SenderType(..)
  , JobType(..)
  , MailerJob(..)
  , MailID
  , EventID
  , Attachment(..)
  , Address(..)
  , SendGridEvent(..)
  , MailGunEvent(..)
  , SocketLabsEvent(..)
  , MailJetEvent(..)
  , Event(..)
  , Mail(..)
  --, unjsonAddress
  ) where

import Control.Monad.Catch
import Data.Aeson.Types
import Data.Data
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T

import DB.Derive
import File.FileID
import HtmlToTxt
import Log.Identifier
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

jobTypeMapper :: [(JobType, T.Text)]
jobTypeMapper = [
    (CleanOldEmails, "clean_old_emails")
  , (PerformServiceTest, "perform_service_test")
  , (CollectServiceTestResult, "collect_service_test_result")
  ]

instance PQFormat JobType where
  pqFormat = pqFormat @T.Text

instance FromSQL JobType where
  type PQBase JobType = PQBase T.Text
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing -> throwM InvalidValue {
        ivValue = v
      , ivValidValues = Just $ map snd jobTypeMapper
      }

instance ToSQL JobType where
  type PQDest JobType = PQBase T.Text
  toSQL tt = toSQL . fromJust $ tt `lookup` jobTypeMapper

data MailerJob = MailerJob {
  mjType      :: !JobType
, mjAttempts  :: !Int32
} deriving (Eq, Ord, Show)

----------------------------------------

newtype MailID = MailID Int64
  deriving (Eq, Ord)
deriving newtype instance Read MailID
deriving newtype instance Show MailID

instance PQFormat MailID where
  pqFormat = pqFormat @Int64

instance Identifier MailID where
  idDefaultLabel     = "mail_id"
  idValue (MailID k) = int64AsStringIdentifier k

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

instance Unjson Address where
  unjsonDef = objectOf $ pure Address
    <*> field "name"
       addrName
       "Name in email address"
    <*> field "email"
       addrEmail
       "Email address"

instance ToJSON Address where
  toJSON = unjsonToJSON unjsonDef
  toEncoding = Aeson.unsafeToEncoding . unjsonToByteStringBuilder unjsonDef

instance PQFormat Address where
  pqFormat = pqFormat @String
instance FromSQL Address where
  type PQBase Address = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Address where
  type PQDest Address = PQDest String
  toSQL = jsonToSQL

instance PQFormat [Address] where
  pqFormat = pqFormat @String
instance FromSQL [Address] where
  type PQBase [Address] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [Address] where
  type PQDest [Address] = PQDest String
  toSQL = jsonToSQL

data Attachment = Attachment {
  attName    :: !String
, attContent :: !(Either B.ByteString FileID)
} deriving (Eq, Ord, Show)

type instance CompositeRow Attachment = (String, Maybe B.ByteString, Maybe FileID)

instance PQFormat Attachment where
  pqFormat = "%mail_attachment"

instance CompositeFromSQL Attachment where
  toComposite (name, mcontent, mfid) = Attachment {
    attName = name
  , attContent = case (mcontent, mfid) of
    (Just content, Nothing) -> Left content
    (Nothing, Just fid) -> Right fid
    _ -> unexpectedError "impossible due to the check constraint"
  }

instance Loggable Attachment where
  logValue Attachment{..} = object $
    ["name" .= attName] ++
    case attContent of
      Left bs -> [
          "type" .= ("string" :: String)
        , "bytesize" .= (B.length bs)
        ]
      Right fid -> [
          "type" .= ("file_id" :: String)
        , identifier fid
        ]
  logDefaultLabel _ = "attachment"

data Mail = Mail {
  mailID          :: !MailID
, mailToken       :: !MagicHash
, mailFrom        :: !Address
, mailTo          :: ![Address]
, mailReplyTo     :: !(Maybe Address)
, mailTitle       :: !String
, mailContent     :: !String
, mailAttachments :: ![Attachment]
, mailServiceTest :: !Bool
, mailAttempts    :: !Int32
} deriving (Eq, Ord, Show)

instance Loggable Mail where
  logValue Mail{..} = object [
      identifier mailID
    , "attachments" .= map logValue mailAttachments
    , "attachment_count" .= length mailAttachments
    , "attempt_count" .= mailAttempts
    , "content" .= htmlToTxt mailContent
    , "from" .= mailFrom
    , "reply_to" .= fromMaybe Null (toJSON <$> mailReplyTo)
    , "service_test" .= mailServiceTest
    , "subject" .= mailTitle
    , "to" .= mailTo
    ]
  logDefaultLabel _ = "mail"

----------------------------------------

newtype EventID = EventID Int64
  deriving (Eq, Ord)
deriving newtype instance Read EventID
deriving newtype instance Show EventID

instance PQFormat EventID where
  pqFormat = pqFormat @Int64

instance Identifier EventID where
  idDefaultLabel      = "mail_event_id"
  idValue (EventID k) = int64AsStringIdentifier k

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

data SocketLabsEvent
  = SL_Opened
  | SL_Delivered
  | SL_Failed Int Int                   -- ^ failure type, failure code
  | SL_Clicked
  | SL_Unsubscribed
  | SL_Complained
    deriving (Eq, Ord, Show, Data, Typeable)

data MailJetEvent
  = MJ_Sent
  | MJ_Open
  | MJ_Click
  | MJ_Bounce_Hard
  | MJ_Bounce_Soft
  | MJ_Spam
  | MJ_Blocked
  | MJ_Unsub
    deriving (Eq, Ord, Show, Data, Typeable)

data Event
  = SendGridEvent !String !SendGridEvent !String   -- ^ email, event, category
  | MailGunEvent !String !MailGunEvent             -- ^ email, event
  | SocketLabsEvent !String !SocketLabsEvent       -- ^ email, event
  | MailJetEvent !String !MailJetEvent             -- ^ email, event
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat Event where
  pqFormat = pqFormat @String
instance FromSQL Event where
  type PQBase Event = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL Event where
  type PQDest Event = PQDest String
  toSQL = jsonToSQL
