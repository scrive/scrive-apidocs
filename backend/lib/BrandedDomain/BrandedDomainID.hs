module BrandedDomain.BrandedDomainID (
    BrandedDomainID
  , unsafeBrandedDomainID
  , unBrandedDomainID
  ) where

import Data.Binary
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude

newtype BrandedDomainID = BrandedDomainID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''BrandedDomainID)

instance FromReqURI BrandedDomainID where
  fromReqURI = maybeRead

instance Binary BrandedDomainID where
  put (BrandedDomainID uid) = put uid
  get = fmap BrandedDomainID get

instance FromSQL BrandedDomainID where
  type PQBase BrandedDomainID = PQBase Int64
  fromSQL mbase = BrandedDomainID <$> fromSQL mbase

instance ToSQL BrandedDomainID where
  type PQDest BrandedDomainID = PQDest Int64
  toSQL (BrandedDomainID n) = toSQL n

unsafeBrandedDomainID :: Int64 -> BrandedDomainID
unsafeBrandedDomainID = BrandedDomainID

unBrandedDomainID :: BrandedDomainID -> Int64
unBrandedDomainID (BrandedDomainID i) = i

unjsonBrandedDomainID :: UnjsonDef BrandedDomainID
unjsonBrandedDomainID = unjsonInvmapR ((maybe (fail "Can't parse DomainID")  return) . maybeRead) (show . unBrandedDomainID :: BrandedDomainID -> String) unjsonDef

instance Unjson BrandedDomainID where
  unjsonDef = unjsonBrandedDomainID
