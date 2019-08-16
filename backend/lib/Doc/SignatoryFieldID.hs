module Doc.SignatoryFieldID (
    SignatoryFieldID
  , unsafeSignatoryFieldID
  , fromSignatoryFieldID
  ) where

import Data.Binary as B
import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

newtype SignatoryFieldID = SignatoryFieldID Int64
  deriving (Eq, Ord, Typeable, Data)
deriving newtype instance Read SignatoryFieldID
deriving newtype instance Show SignatoryFieldID

instance PQFormat SignatoryFieldID where
  pqFormat = pqFormat @Int64

instance FromReqURI SignatoryFieldID where
  fromReqURI = maybeRead . T.pack

instance Binary SignatoryFieldID where
  put (SignatoryFieldID did) = put did
  get = fmap SignatoryFieldID B.get

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
