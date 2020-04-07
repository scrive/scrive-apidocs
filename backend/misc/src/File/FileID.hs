module File.FileID (
    FileID
  , unsafeFileID
  , fromFileID
  ) where

import Data.Hashable
import Data.Int
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype FileID = FileID Int64
  deriving (Eq, Ord, Hashable, Typeable)
deriving newtype instance Read FileID
deriving newtype instance Show FileID
deriving newtype instance TextShow FileID

instance PQFormat FileID where
  pqFormat = pqFormat @Int64

instance Identifier FileID where
  idDefaultLabel = "file_id"
  idValue (FileID k) = int64AsStringIdentifier k

instance FromReqURI FileID where
  fromReqURI = maybeRead . T.pack

instance Unjson FileID where
  unjsonDef = unjsonInvmapR
    (maybe (fail "Can't parse FileID") return . maybeRead . T.pack)
    show
    unjsonDef

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
