module SMS.Data where

import Data.Int

import DB.Derive
import Data.Data

newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''ShortMessageID)
$(newtypeDeriveConvertible ''ShortMessageID)

data ShortMessage = ShortMessage {
    smID         :: ShortMessageID
  , smOriginator :: String
  , smMSISDN     :: String
  , smBody       :: String
  , smData       :: String
  } deriving (Eq, Ord, Show)

newtype SMSEventID = SMSEventID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SMSEventID)
$(newtypeDeriveConvertible ''SMSEventID)

data SMSEventType =
    SMSDelivered
  | SMSUndelivered String          -- ^ reason
    deriving (Eq, Ord, Show, Data, Typeable)

data SMSEvent = SMSEvent String SMSEventType -- ^ phone number, event
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| SMSEvent |])
