module Theme.ThemeID (
    ThemeID
  , unsafeThemeID
  , fromThemeID
  ) where

import Control.Applicative
import Data.Binary
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes hiding (Binary, put)
import Happstack.Server

import DB.Derive
import Utils.Read

newtype ThemeID = ThemeID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''ThemeID)

instance FromReqURI ThemeID where
  fromReqURI = maybeRead

instance Binary ThemeID where
  put (ThemeID cid) = put cid
  get = fmap ThemeID get

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


-- TODO Fix to stop using read. Gracjan is working on that in unjson library
unjsonThemeID :: UnjsonDef ThemeID
unjsonThemeID = unjsonInvmapR ((maybe (fail "Can't parse ThemeID")  (return . ThemeID) . maybeReadInt64)) (show . fromThemeID :: ThemeID -> String) unjsonDef

instance Unjson ThemeID where
  unjsonDef = unjsonThemeID
