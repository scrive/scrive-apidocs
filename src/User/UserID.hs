module User.UserID (
    UserID
  , unsafeUserID
  , unUserID
  ) where

import Control.Applicative
import Data.Binary
import Data.Int
import Data.Typeable
import Database.PostgreSQL.PQTypes hiding (Binary, put)
import Happstack.Server

import DB.Derive
import Utils.Read

newtype UserID = UserID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

instance Binary UserID where
  put (UserID uid) = put uid
  get = fmap UserID get

instance FromSQL UserID where
  type PQBase UserID = PQBase Int64
  fromSQL mbase = UserID <$> fromSQL mbase

instance ToSQL UserID where
  type PQDest UserID = PQDest Int64
  toSQL (UserID n) = toSQL n

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

unUserID :: UserID -> Int64
unUserID (UserID i) = i
