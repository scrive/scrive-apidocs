module Company.CompanyID (
    CompanyID
  , unsafeCompanyID
  ) where

import Control.Monad
import Data.Int
import Data.SafeCopy
import Data.Typeable
import Happstack.Server

import Crypto.RNG
import DB.Derive
import Misc

newtype CompanyID = CompanyID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''CompanyID)
$(newtypeDeriveUnderlyingReadShow ''CompanyID)

$(deriveSafeCopy 0 'base ''CompanyID)

instance FromReqURI CompanyID where
  fromReqURI = readM

instance Random CompanyID where
  random = CompanyID `liftM` randomR (10000000, 10000000000)

unsafeCompanyID :: Int64 -> CompanyID
unsafeCompanyID = CompanyID
