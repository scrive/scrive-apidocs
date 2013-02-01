module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  , fromCompanyID
  ) where

import Data.Int
import Data.Typeable
import Data.Binary
import Happstack.Server

import DB.Derive
import Utils.Read

newtype CompanyID = CompanyID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = maybeRead

instance Binary CompanyID where
  put (CompanyID cid) = put cid
  get = fmap CompanyID get

unsafeCompanyID :: Int64 -> CompanyID
unsafeCompanyID = CompanyID

fromCompanyID :: CompanyID -> Int64
fromCompanyID (CompanyID cid) = cid