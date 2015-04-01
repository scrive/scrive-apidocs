module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  , fromCompanyID
  ) where

import Data.Binary
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes hiding (Binary, put)
import Happstack.Server

import DB.Derive
import KontraPrelude
import Utils.Read

newtype CompanyID = CompanyID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = maybeRead

instance Binary CompanyID where
  put (CompanyID cid) = put cid
  get = fmap CompanyID get

instance FromSQL CompanyID where
  type PQBase CompanyID = PQBase Int64
  fromSQL mbase = CompanyID <$> fromSQL mbase

instance ToSQL CompanyID where
  type PQDest CompanyID = PQDest Int64
  toSQL (CompanyID n) = toSQL n

unsafeCompanyID :: Int64 -> CompanyID
unsafeCompanyID = CompanyID

fromCompanyID :: CompanyID -> Int64
fromCompanyID (CompanyID cid) = cid


unjsonCompanyID :: UnjsonDef CompanyID
unjsonCompanyID = unjsonInvmapR ((maybe (fail "Can't parse CompanyID")  return) . maybeRead) (show . fromCompanyID :: CompanyID -> String) unjsonDef

instance Unjson CompanyID where
  unjsonDef = unjsonCompanyID
