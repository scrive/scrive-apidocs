module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  , fromCompanyID
  ) where

import Control.Applicative
import Data.Int
import Data.Typeable
import Data.Binary
import Database.PostgreSQL.PQTypes hiding (Binary, put)
import Happstack.Server

import DB.Derive
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
