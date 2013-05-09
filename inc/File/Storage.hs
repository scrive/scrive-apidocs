

module File.Storage
    ( getFileContents
    , getFileIDContents
    ) where

import Control.Monad.Trans
import DB
import qualified Amazon as AWS
import File.Model
import File.File
import qualified Data.ByteString as BS
import qualified MemCache as MemCache
import qualified Log


{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: (Log.MonadLog m, MonadIO m, AWS.AmazonMonad m) => File -> m BS.ByteString
getFileContents file = do
  ac <- AWS.getAmazonConfig
  mcontent <- MemCache.get (fileid file) (AWS.fileCache ac)
  case mcontent of
      Just content -> return content
      Nothing -> do
        mcontentAWS <- liftIO $ AWS.getFileContents (AWS.mkAWSAction $ AWS.amazonConfig ac) file
        case mcontentAWS of
          Nothing -> do
            Log.debug $ "Couldn't get content for file " ++ show (fileid file) ++ ", returning empty ByteString."
            return BS.empty
          Just contentAWS -> do
            MemCache.put (fileid file) contentAWS (AWS.fileCache ac)
            return contentAWS

getFileIDContents :: (MonadDB m, MonadIO m, AWS.AmazonMonad m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  mfile <- dbQuery $ GetFileByFileID fid
  case mfile of
    Just file -> getFileContents file
    Nothing -> return BS.empty

