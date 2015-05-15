module File.FileID (
    FileID
  , unsafeFileID
  , fromFileID
  ) where

import Data.Int
import Data.Typeable
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude
import Log.Identifier

newtype FileID = FileID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''FileID)

instance Identifier FileID Int64 where
  gidentifier f n = f "file_id" .= fmap (\(FileID k) -> k) n

instance FromReqURI FileID where
  fromReqURI = maybeRead

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
