module File.FileID (
    FileID
  , unsafeFileID
  , fromFileID
  ) where

import Data.Aeson
import Data.Hashable
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server

import KontraPrelude
import Log.Identifier

newtype FileID = FileID Int64
  deriving (Eq, Ord, Hashable, PQFormat, Typeable)
deriving newtype instance Read FileID
deriving newtype instance Show FileID

instance Identifier FileID Int64 where
  idDefaultLabel _ = "file_id"
  idValue (FileID k) = toJSON k

instance FromReqURI FileID where
  fromReqURI = maybeRead

instance Unjson FileID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) show unjsonDef

instance FromSQL FileID where
  type PQBase FileID = PQBase Int64
  fromSQL mbase = FileID <$> fromSQL mbase

instance ToSQL FileID where
  type PQDest FileID = PQDest Int64
  toSQL (FileID n) = toSQL n

unsafeFileID :: Int64 -> FileID
unsafeFileID = FileID

fromFileID :: FileID -> Int64
fromFileID (FileID fid) = fid
