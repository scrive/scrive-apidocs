module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  ) where

import Data.Int
import Data.Typeable
import Happstack.Server

import DB.Derive
import Utils.Read

newtype CompanyID = CompanyID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = maybeRead

unsafeCompanyID :: Int64 -> CompanyID
unsafeCompanyID = CompanyID
