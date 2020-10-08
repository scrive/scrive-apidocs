module Auth.Session.SessionID (
    -- Note: previously the SessionID data constructor was hidden but we need to
    -- export it now for the Identifier instance kontrakcja's lib/Session/SessionID.hs
    SessionID(..)
  , tempSessionID
  ) where

import Data.Aeson
import Data.Int
import Database.PostgreSQL.PQTypes

newtype SessionID = SessionID Int64
  deriving (Eq, Ord, Read, Show, TextShow, ToJSON)

instance PQFormat SessionID where
  pqFormat = pqFormat @Int64

instance FromSQL SessionID where
  type PQBase SessionID = PQBase Int64
  fromSQL mbase = SessionID <$> fromSQL mbase
instance ToSQL SessionID where
  type PQDest SessionID = PQDest Int64
  toSQL (SessionID n) = toSQL n

tempSessionID :: SessionID
tempSessionID = SessionID 0
