module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  ) where

import Control.Monad
import Data.Data
import Data.Int
import Happstack.Data
import Happstack.Server
import Happstack.Util.Common

import Crypto.RNG
import DB.Derive

newtype CompanyID = CompanyID Int64
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = readM

instance Random CompanyID where
  random = CompanyID `liftM` randomR (10000000, 10000000000)

unsafeCompanyID :: Int64 -> CompanyID
unsafeCompanyID = CompanyID

-- blah, old migrations

newtype CompanyID_0 = CompanyID_0 Int
  deriving (Eq, Ord, Typeable)

instance Version CompanyID where
  mode = extension 2 (Proxy :: Proxy CompanyID_0)

instance Version CompanyID_0

instance Migrate CompanyID_0 CompanyID where
  migrate (CompanyID_0 n) = CompanyID (fromIntegral n)

$(deriveSerializeFor [''CompanyID, ''CompanyID_0])
