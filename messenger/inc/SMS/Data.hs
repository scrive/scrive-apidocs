module SMS.Data where

import Data.Int

import DB.Derive
import MagicHash


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
