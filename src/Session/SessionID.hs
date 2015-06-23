module Session.SessionID (
    SessionID
  , tempSessionID
  ) where

import Data.Int

import DB
import KontraPrelude
import Log.Identifier

newtype SessionID = SessionID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SessionID)

instance Identifier SessionID Int64 where
  gidentifier f n = f "session_id" .= fmap (\(SessionID k) -> k) n

instance FromSQL SessionID where
  type PQBase SessionID = PQBase Int64
  fromSQL mbase = SessionID <$> fromSQL mbase
instance ToSQL SessionID where
  type PQDest SessionID = PQDest Int64
  toSQL (SessionID n) = toSQL n

tempSessionID :: SessionID
tempSessionID = SessionID 0
