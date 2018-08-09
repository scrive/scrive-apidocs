module Session.SessionID (
    SessionID
  , tempSessionID
  ) where

import Data.Aeson
import Data.Int

import DB
import Log.Identifier

newtype SessionID = SessionID Int64
  deriving (Eq, Ord)
deriving newtype instance Read SessionID
deriving newtype instance Show SessionID

instance PQFormat SessionID where
  pqFormat = pqFormat @Int64

instance Identifier SessionID Int64 where
  idDefaultLabel _ = "session_id"
  idValue (SessionID k) = toJSON . show $ k

instance FromSQL SessionID where
  type PQBase SessionID = PQBase Int64
  fromSQL mbase = SessionID <$> fromSQL mbase
instance ToSQL SessionID where
  type PQDest SessionID = PQDest Int64
  toSQL (SessionID n) = toSQL n

tempSessionID :: SessionID
tempSessionID = SessionID 0
