module SMS.Types (
    JobType(..)
  , MessengerJob(..)
  , SMSProvider(..)
  , codeFromSMSProvider
  , smsProviderFromCode
  , ShortMessageID
  , ShortMessage(..)
  , SMSEventID
  , SMSEventType(..)
  , SMSEvent(..)
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes
import GHC.Generics

import DB.Derive
import Log.Identifier
import Utils.Enum
import Utils.List

data JobType
  = CleanOldSMSes
  deriving (Eq, Ord, Show)

jobTypeMapper :: [(JobType, Text)]
jobTypeMapper = [(CleanOldSMSes, "clean_old_smses")]

instance PQFormat JobType where
  pqFormat = pqFormat @Text

instance FromSQL JobType where
  type PQBase JobType = PQBase Text
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing ->
        throwM InvalidValue { ivValue = v, ivValidValues = Just $ map snd jobTypeMapper }

instance ToSQL JobType where
  type PQDest JobType = PQBase Text
  toSQL tt = toSQL . fromJust $ tt `lookup` jobTypeMapper

data MessengerJob = MessengerJob
  { mjType      :: JobType
  , mjAttempts  :: Int32
  } deriving (Eq, Ord, Show)

----------------------------------------

data SMSProvider = SMSDefault
                 | SMSTeliaCallGuide
  deriving (Bounded, Enum, Eq, Ord, Show, Read, Generic)

-- For logging
instance ToJSON SMSProvider

instance PQFormat SMSProvider where
  pqFormat = pqFormat @Int16

instance FromSQL SMSProvider where
  type PQBase SMSProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SMSDefault
      2 -> return SMSTeliaCallGuide
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL SMSProvider where
  type PQDest SMSProvider = PQDest Int16
  toSQL SMSDefault        = toSQL (1 :: Int16)
  toSQL SMSTeliaCallGuide = toSQL (2 :: Int16)

codeFromSMSProvider :: SMSProvider -> Text
codeFromSMSProvider SMSDefault        = "default"
codeFromSMSProvider SMSTeliaCallGuide = "telia_call_guide"

smsProviderFromCode :: Text -> Maybe SMSProvider
smsProviderFromCode s = find ((== s) . codeFromSMSProvider) allValues

----------------------------------------

newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord)
deriving newtype instance Read ShortMessageID
deriving newtype instance Show ShortMessageID

instance PQFormat ShortMessageID where
  pqFormat = pqFormat @Int64

instance Identifier ShortMessageID where
  idDefaultLabel = "sms_id"
  idValue (ShortMessageID k) = int64AsStringIdentifier k

instance FromSQL ShortMessageID where
  type PQBase ShortMessageID = PQBase Int64
  fromSQL mbase = ShortMessageID <$> fromSQL mbase

instance ToSQL ShortMessageID where
  type PQDest ShortMessageID = PQDest Int64
  toSQL (ShortMessageID n) = toSQL n

data ShortMessage = ShortMessage
  { smID         :: ShortMessageID
  , smProvider   :: SMSProvider
  , smOriginator :: Text
  , smMSISDN     :: Text
  , smBody       :: Text
  , smAttempts   :: Int32
  } deriving (Eq, Ord, Show)

instance Loggable ShortMessage where
  logValue ShortMessage {..} = object
    [ identifier smID
    , "provider" .= show smProvider
    , "originator" .= smOriginator
    , "msisdn" .= smMSISDN -- original/non-clean format
    , "body" .= smBody
    , "attempts" .= smAttempts
    ]
  logDefaultLabel _ = "short_message"

----------------------------------------

newtype SMSEventID = SMSEventID Int64
  deriving (Eq, Ord)
deriving newtype instance Read SMSEventID
deriving newtype instance Show SMSEventID

instance PQFormat SMSEventID where
  pqFormat = pqFormat @Int64

instance Identifier SMSEventID where
  idDefaultLabel = "sms_event_id"
  idValue (SMSEventID k) = int64AsStringIdentifier k

instance FromSQL SMSEventID where
  type PQBase SMSEventID = PQBase Int64
  fromSQL mbase = SMSEventID <$> fromSQL mbase

instance ToSQL SMSEventID where
  type PQDest SMSEventID = PQDest Int64
  toSQL (SMSEventID n) = toSQL n

data SMSEventType
  = SMSDelivered
  | SMSUndelivered !Text -- ^ reason
    deriving (Eq, Ord, Show, Data, Typeable)

data SMSEvent = SMSEvent !Text !SMSEventType -- ^ phone number, event
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat SMSEvent where
  pqFormat = pqFormat @String

instance FromSQL SMSEvent where
  type PQBase SMSEvent = PQBase String
  fromSQL = jsonFromSQL

instance ToSQL SMSEvent where
  type PQDest SMSEvent = PQDest String
  toSQL = jsonToSQL
