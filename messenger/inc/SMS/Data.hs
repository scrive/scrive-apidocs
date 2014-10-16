module SMS.Data where

import Control.Applicative
import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes

import DB.Derive

newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''ShortMessageID)

instance FromSQL ShortMessageID where
  type PQBase ShortMessageID = PQBase Int64
  fromSQL mbase = ShortMessageID <$> fromSQL mbase

instance ToSQL ShortMessageID where
  type PQDest ShortMessageID = PQDest Int64
  toSQL (ShortMessageID n) = toSQL n

data ShortMessage = ShortMessage {
    smID         :: ShortMessageID
  , smOriginator :: String
  , smMSISDN     :: String
  , smBody       :: String
  , smData       :: String
  , smAttempt    :: Int32
  } deriving (Eq, Ord, Show)

newtype SMSEventID = SMSEventID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SMSEventID)

instance FromSQL SMSEventID where
  type PQBase SMSEventID = PQBase Int64
  fromSQL mbase = SMSEventID <$> fromSQL mbase

instance ToSQL SMSEventID where
  type PQDest SMSEventID = PQDest Int64
  toSQL (SMSEventID n) = toSQL n

data SMSEventType =
    SMSDelivered
  | SMSUndelivered String          -- ^ reason
    deriving (Eq, Ord, Show, Data, Typeable)

data SMSEvent = SMSEvent String SMSEventType -- ^ phone number, event
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat SMSEvent where
  pqFormat _ = pqFormat (undefined::String)

instance FromSQL SMSEvent where
  type PQBase SMSEvent = PQBase String
  fromSQL = jsonFromSQL

instance ToSQL SMSEvent where
  type PQDest SMSEvent = PQDest String
  toSQL = jsonToSQL
