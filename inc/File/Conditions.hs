

module File.Conditions
where

import DB.SQL2
import Control.Monad.State.Class
import Data.Typeable
import DB.SQL
import File.FileID
import Text.JSON.Gen
import MinutesTime

-- This is the part where we define all possible wrongs about a file.

data FileDoesNotExist = FileDoesNotExist FileID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FileDoesNotExist where
  toJSValue (FileDoesNotExist fid) = runJSONGen $ do
                     value "message" ("File does not exists" :: String)
                     value "file_id" (show fid)

instance KontraException FileDoesNotExist

sqlWhereFileIDIs :: (MonadState v m, SqlWhere v)
                     => FileID -> m ()
sqlWhereFileIDIs fid =
  sqlWhereE (FileDoesNotExist fid) ("files.id = " <?> fid)


data FileWasPurged = FileWasPurged FileID MinutesTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue FileWasPurged where
  toJSValue (FileWasPurged fid time) = runJSONGen $ do
                     value "message" ("File was purged from the system and is no longer available" :: String)
                     value "file_id" (show fid)
                     value "purge_time" (show time)

instance KontraException FileWasPurged

sqlWhereFileWasNotPurged :: (MonadState v m, SqlWhere v)
                     => m ()
sqlWhereFileWasNotPurged =
  sqlWhereEVV (FileWasPurged,"files.id","files.purge_time") ("files.purge_time IS NULL")
