module File.Storage (
    getFileContents
  , getFileIDContents
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Time
import Log
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS

import Database.Redis.Helpers
import DB
import File.File
import File.Model
import qualified Amazon as AWS
import qualified Database.Redis.Cache as RC
import qualified MemCache as MemCache

-- | Gets file content from somewere (Amazon for now), putting it to cache and
-- returning as BS.
getFileContents
  :: (MonadBaseControl IO m, MonadMask m, MonadLog m, AWS.AmazonMonad m)
  => File
  -> m BS.ByteString
getFileContents file = do
  ac <- AWS.getAmazonConfig
  MemCache.fetch_ (AWS.awsLocalCache ac) (fileid file) $ do
    RC.mfetch (AWS.awsGlobalCache ac) rkey
      AWS.getFileFromRedis
      (AWS.getFileContents (AWS.mkAWSAction $ AWS.awsConfig ac) file)
  where
   rkey = mkRedisKey [
       "files"
     , BS.pack . show $ fileid file
     , Base16.encode $ filechecksum file
     ]

getFileIDContents :: (MonadDB m, MonadIO m, MonadMask m, MonadLog m, MonadBaseControl IO m, AWS.AmazonMonad m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  start <- liftIO getCurrentTime
  file <- dbQuery $ GetFileByFileID fid
  result <- getFileContents file
  stop <- liftIO getCurrentTime
  logInfo "getFileIDContents timing" $ object ["duration" .= (realToFrac $ diffUTCTime stop start :: Double)]
  return result
