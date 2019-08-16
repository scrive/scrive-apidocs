module Theme.ThemeID (
    ThemeID
  , unsafeThemeID
  , fromThemeID
  ) where

import Data.Binary as B
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype ThemeID = ThemeID Int64
  deriving (Eq, Ord, Typeable)
deriving newtype instance Read ThemeID
deriving newtype instance Show ThemeID
deriving newtype instance TextShow ThemeID

instance PQFormat ThemeID where
  pqFormat = pqFormat @Int64

instance FromReqURI ThemeID where
  fromReqURI = maybeRead . T.pack

instance Binary ThemeID where
  put (ThemeID cid) = put cid
  get = fmap ThemeID B.get

instance FromSQL ThemeID where
  type PQBase ThemeID = PQBase Int64
  fromSQL mbase = ThemeID <$> fromSQL mbase

instance ToSQL ThemeID where
  type PQDest ThemeID = PQDest Int64
  toSQL (ThemeID n) = toSQL n

unsafeThemeID:: Int64 -> ThemeID
unsafeThemeID = ThemeID

fromThemeID:: ThemeID -> Int64
fromThemeID (ThemeID tid) = tid


unjsonThemeID :: UnjsonDef ThemeID
unjsonThemeID = unjsonInvmapR
  ((maybe (fail "Can't parse ThemeID")  return) . maybeRead . T.pack)
  (show . fromThemeID :: ThemeID -> String)
  unjsonDef

instance Unjson ThemeID where
  unjsonDef = unjsonThemeID

instance Identifier ThemeID where
  idDefaultLabel      = "theme_id"
  idValue (ThemeID k) = int64AsStringIdentifier k
