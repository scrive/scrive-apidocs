module User.UserID (
    UserID
  , unsafeUserID
  , unUserID
  ) where

import Data.Binary
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes hiding (put)
import Happstack.Server

import DB.Derive
import KontraPrelude
import Log.Identifier

newtype UserID = UserID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

instance Binary UserID where
  put (UserID uid) = put uid
  get = fmap UserID get

instance Identifier UserID Int64 where
  gidentifier f n = f "user_id" .= fmap (\(UserID k) -> k) n

instance Unjson UserID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse UserID")  return) . maybeRead) show  unjsonDef

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
