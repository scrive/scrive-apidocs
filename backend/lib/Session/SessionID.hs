module Session.SessionID (
    SessionID
  , tempSessionID
  ) where

import Data.Int

import DB
import Log.Identifier

newtype SessionID = SessionID Int64
  deriving (Eq, Ord)
deriving newtype instance Read SessionID
deriving newtype instance Show SessionID
deriving newtype instance TextShow SessionID

instance PQFormat SessionID where
  pqFormat = pqFormat @Int64

instance Identifier SessionID where
  idDefaultLabel        = "session_id"
  idValue (SessionID k) = int64AsStringIdentifier k

instance FromSQL SessionID where
  type PQBase SessionID = PQBase Int64
  fromSQL mbase = SessionID <$> fromSQL mbase
instance ToSQL SessionID where
  type PQDest SessionID = PQDest Int64
  toSQL (SessionID n) = toSQL n

tempSessionID :: SessionID
tempSessionID = SessionID 0
