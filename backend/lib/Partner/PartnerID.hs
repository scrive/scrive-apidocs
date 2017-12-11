module Partner.PartnerID (
    PartnerID
  , unsafePartnerID
  , unPartnerID
  ) where

import Data.Aeson
import Data.Binary
import Data.Default (Default(..))
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server

import KontraPrelude
import Log.Identifier

newtype PartnerID = PartnerID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
deriving newtype instance Read PartnerID
deriving newtype instance Show PartnerID

instance Default PartnerID where
  def = unsafePartnerID 0

instance FromReqURI PartnerID where
  fromReqURI = maybeRead

instance Binary PartnerID where
  put (PartnerID uid) = put uid
  get = fmap PartnerID get

instance FromSQL PartnerID where
  type PQBase PartnerID = PQBase Int64
  fromSQL mbase = PartnerID <$> fromSQL mbase

instance ToSQL PartnerID where
  type PQDest PartnerID = PQDest Int64
  toSQL (PartnerID n) = toSQL n

unsafePartnerID :: Int64 -> PartnerID
unsafePartnerID = PartnerID

unPartnerID :: PartnerID -> Int64
unPartnerID (PartnerID i) = i

unjsonPartnerID :: UnjsonDef PartnerID
unjsonPartnerID = unjsonInvmapR ((maybe (fail "Can't parse PartnerID")  return) . maybeRead) (show . unPartnerID :: PartnerID -> String) unjsonDef

instance Unjson PartnerID where
  unjsonDef = unjsonPartnerID

instance Identifier PartnerID Int64 where
  idDefaultLabel _ = "partner_id"
  idValue = toJSON . unPartnerID
