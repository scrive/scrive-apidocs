module Partner.PartnerID (
    PartnerID
  , defaultPartnerID
  , unsafePartnerID
  , unPartnerID
  ) where

import Data.Binary as B
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype PartnerID = PartnerID Int64
  deriving (Eq, Ord, Typeable)
deriving newtype instance Read PartnerID
deriving newtype instance Show PartnerID

instance PQFormat PartnerID where
  pqFormat = pqFormat @Int64

defaultPartnerID :: PartnerID
defaultPartnerID = unsafePartnerID 0

instance FromReqURI PartnerID where
  fromReqURI = maybeRead . T.pack

instance Binary PartnerID where
  put (PartnerID uid) = put uid
  get = fmap PartnerID B.get

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
unjsonPartnerID = unjsonInvmapR
  ((maybe (fail "Can't parse PartnerID") return) . maybeRead . T.pack)
  (show . unPartnerID :: PartnerID -> String)
  unjsonDef

instance Unjson PartnerID where
  unjsonDef = unjsonPartnerID

instance Identifier PartnerID where
  idDefaultLabel = "partner_id"
  idValue        = int64AsStringIdentifier . unPartnerID
