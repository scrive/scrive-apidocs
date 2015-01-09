module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  , fromCompanyID
  ) where

import Control.Applicative
import Data.Binary
import Data.Functor.Invariant
import Data.Int
import Data.Typeable
import Data.Unjson
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


-- TODO Fix to stop using read. Gracjan is working on that in unjson library
unjsonThemeID :: UnjsonDef CompanyID
unjsonThemeID = invmap (CompanyID . read :: String -> CompanyID) (show . fromCompanyID :: CompanyID -> String) unjsonDef

instance Unjson CompanyID where
  unjsonDef = unjsonThemeID
