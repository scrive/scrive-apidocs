module User.UserID (
    UserID
  , unsafeUserID
  , unUserID
  ) where

import Data.Aeson
import Data.Binary as B
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype UserID = UserID Int64
  deriving (Eq, Ord, Typeable)
deriving newtype instance Read UserID
deriving newtype instance Show UserID
deriving newtype instance TextShow UserID

instance PQFormat UserID where
  pqFormat = pqFormat @Int64

instance FromReqURI UserID where
  fromReqURI = maybeRead . T.pack

instance Binary UserID where
  put (UserID uid) = put uid
  get = fmap UserID B.get

instance Identifier UserID where
  idDefaultLabel = "user_id"
  idValue        = int64AsStringIdentifier . unUserID

instance Unjson UserID where
  unjsonDef =
    unjsonInvmapR ((maybe (fail "Can't parse UserID") return) . maybeRead) showt unjsonDef

instance ToJSON UserID where
  toJSON (UserID n) = toJSON $ show n

instance FromJSON UserID where
  parseJSON v = do
    uidStr <- parseJSON v
    case maybeRead uidStr of
      Nothing  -> fail "Could not parse User ID"
      Just uid -> return uid

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
