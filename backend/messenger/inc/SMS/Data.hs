module SMS.Data (
    JobType(..)
  , MessengerJob(..)
  , SMSProvider(..)
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
import qualified Data.Text as T

import DB.Derive
import KontraPrelude
import Log.Identifier
import Utils.List

data JobType
  = CleanOldSMSes
  deriving (Eq, Ord, Show)

jobTypeMapper :: [(JobType, T.Text)]
jobTypeMapper = [
    (CleanOldSMSes, "clean_old_smses")
  ]

instance PQFormat JobType where
  pqFormat = const $ pqFormat (undefined::T.Text)

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

data MessengerJob = MessengerJob {
  mjType      :: !JobType
, mjAttempts  :: !Int32
} deriving (Eq, Ord, Show)

----------------------------------------

data SMSProvider = SMSDefault
                 | SMSTeliaCallGuide
   deriving (Eq, Ord, Show, Read)

instance PQFormat SMSProvider where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL SMSProvider where
  type PQBase SMSProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SMSDefault
      2 -> return SMSTeliaCallGuide
      _ -> throwM RangeError {
        reRange = [(1,2)]
      , reValue = n
      }

instance ToSQL SMSProvider where
  type PQDest SMSProvider = PQDest Int16
  toSQL SMSDefault        = toSQL (1::Int16)
  toSQL SMSTeliaCallGuide = toSQL (2::Int16)

----------------------------------------

newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''ShortMessageID)

instance Identifier ShortMessageID Int64 where
  idDefaultLabel _ = "sms_id"
  idValue (ShortMessageID k) = toJSON k

instance FromSQL ShortMessageID where
  type PQBase ShortMessageID = PQBase Int64
  fromSQL mbase = ShortMessageID <$> fromSQL mbase

instance ToSQL ShortMessageID where
  type PQDest ShortMessageID = PQDest Int64
  toSQL (ShortMessageID n) = toSQL n

data ShortMessage = ShortMessage {
  smID         :: !ShortMessageID
, smProvider   :: !SMSProvider
, smOriginator :: !String
, smMSISDN     :: !String
, smBody       :: !String
, smData       :: !String
, smAttempts   :: !Int32
} deriving (Eq, Ord, Show)

instance ToJSON ShortMessage where
  toJSON ShortMessage{..} = object [
      identifier_ smID
    , "provider"   .= show smProvider
    , "originator" .= smOriginator
    , "msisdn"     .= smMSISDN -- original/non-clean format
    , "body"       .= smBody
    , "data"       .= smData
    , "attempts"   .= smAttempts
    ]

instance Loggable ShortMessage where
  logValue = toJSON
  logDefaultLabel _ = "short_message"

----------------------------------------

newtype SMSEventID = SMSEventID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SMSEventID)

instance Identifier SMSEventID Int64 where
  idDefaultLabel _ = "sms_event_id"
  idValue (SMSEventID k) = toJSON k

instance FromSQL SMSEventID where
  type PQBase SMSEventID = PQBase Int64
  fromSQL mbase = SMSEventID <$> fromSQL mbase

instance ToSQL SMSEventID where
  type PQDest SMSEventID = PQDest Int64
  toSQL (SMSEventID n) = toSQL n

data SMSEventType
  = SMSDelivered
  | SMSUndelivered !String -- ^ reason
    deriving (Eq, Ord, Show, Data, Typeable)

data SMSEvent = SMSEvent !String !SMSEventType -- ^ phone number, event
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat SMSEvent where
  pqFormat = const $ pqFormat (undefined::String)

instance FromSQL SMSEvent where
  type PQBase SMSEvent = PQBase String
  fromSQL = jsonFromSQL

instance ToSQL SMSEvent where
  type PQDest SMSEvent = PQDest String
  toSQL = jsonToSQL
