module File.Conditions where

import Control.Monad.State.Class
import Data.Typeable
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder
import Text.JSON.Gen

import File.FileID
import MinutesTime

-- This is the part where we define all possible wrongs about a file.

newtype FileDoesNotExist = FileDoesNotExist FileID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FileDoesNotExist where
  toJSValue (FileDoesNotExist fid) = runJSONGen $ do
    value "message" ("File does not exists" :: String)
    value "file_id" (show fid)

instance DBExtraException FileDoesNotExist

sqlWhereFileIDIs :: (MonadState v m, SqlWhere v) => FileID -> m ()
sqlWhereFileIDIs fid = sqlWhereE (FileDoesNotExist fid) ("files.id =" <?> fid)

data FileWasPurged = FileWasPurged FileID UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FileWasPurged where
  toJSValue (FileWasPurged fid time) = runJSONGen $ do
    value "message"
          ("File was purged from the system and is no longer available" :: String)
    value "file_id"     (show fid)
    value "purged_time" (show time)

instance DBExtraException FileWasPurged

sqlWhereFileWasNotPurged :: (MonadState v m, SqlWhere v) => m ()
sqlWhereFileWasNotPurged =
  sqlWhereEVV (FileWasPurged, "files.id", "files.purged_time") "files.purged_time IS NULL"
