module Session.SessionID (
    SessionID
  , tempSessionID
  ) where

import Control.Applicative
import Data.Int

import DB

newtype SessionID = SessionID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SessionID)

instance FromSQL SessionID where
  type PQBase SessionID = PQBase Int64
  fromSQL mbase = SessionID <$> fromSQL mbase
instance ToSQL SessionID where
  type PQDest SessionID = PQDest Int64
  toSQL (SessionID n) = toSQL n

tempSessionID :: SessionID
tempSessionID = SessionID 0
