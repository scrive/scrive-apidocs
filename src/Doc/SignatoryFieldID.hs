module Doc.SignatoryFieldID (
    SignatoryFieldID
  , unsafeSignatoryFieldID
  , fromSignatoryFieldID
  ) where

import Control.Applicative
import Data.Binary
import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes hiding (Binary, put)
import Happstack.Server

import DB.Derive
import Utils.Read

newtype SignatoryFieldID = SignatoryFieldID Int64
  deriving (Eq, Ord, PQFormat, Typeable, Data)
$(newtypeDeriveUnderlyingReadShow ''SignatoryFieldID)

instance FromReqURI SignatoryFieldID where
  fromReqURI = maybeRead

instance Binary SignatoryFieldID where
  put (SignatoryFieldID did) = put did
  get = fmap SignatoryFieldID get

instance FromSQL SignatoryFieldID where
  type PQBase SignatoryFieldID = PQBase Int64
  fromSQL mbase = SignatoryFieldID <$> fromSQL mbase
instance ToSQL SignatoryFieldID where
  type PQDest SignatoryFieldID = PQDest Int64
  toSQL (SignatoryFieldID n) = toSQL n

unsafeSignatoryFieldID :: Int64 -> SignatoryFieldID
unsafeSignatoryFieldID = SignatoryFieldID

fromSignatoryFieldID :: SignatoryFieldID -> Int64
fromSignatoryFieldID (SignatoryFieldID did) = did
