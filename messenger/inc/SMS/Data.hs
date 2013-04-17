module SMS.Data where

import Data.Int

import DB.Derive
import MagicHash
import Data.Data


newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''ShortMessageID)
$(newtypeDeriveConvertible ''ShortMessageID)

data ShortMessage = ShortMessage {
    smID         :: ShortMessageID
  , smToken      :: MagicHash
  , smOriginator :: String
  , smMSISDN     :: String
  , smBody       :: String
  } deriving (Eq, Ord, Show)

newtype SMSEventID = SMSEventID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SMSEventID)
$(newtypeDeriveConvertible ''SMSEventID)

data GlobalMouthEvent =
    GM_Delivered
  | GM_Undelivered String          -- ^ reason
    deriving (Eq, Ord, Show, Data, Typeable)

data SMSEvent =
    GlobalMouthEvent String GlobalMouthEvent -- ^ phone number, event
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| SMSEvent |])
