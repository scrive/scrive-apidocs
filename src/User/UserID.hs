module User.UserID (
    UserID
  , unsafeUserID
  , unUserID
  ) where

import Control.Applicative
import Data.Int
import Data.Typeable
import Database.PostgreSQL.PQTypes
import DB.Derive
import Happstack.Server
import Utils.Read

newtype UserID = UserID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

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
