module File.FileID (
    FileID
  , unsafeFileID
  ) where

import Data.Int
import Data.Typeable
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude
import Utils.Read

newtype FileID = FileID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''FileID)

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
