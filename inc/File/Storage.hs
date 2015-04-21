module File.Storage (
    getFileContents
  , getFileIDContents
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import qualified Data.ByteString as BS

import DB
import File.File
import File.Model
import KontraPrelude
import Log
import qualified Amazon as AWS
import qualified MemCache as MemCache

{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: (MonadLog m, MonadBase IO m, AWS.AmazonMonad m) => File -> m BS.ByteString
getFileContents file = do
  ac <- AWS.getAmazonConfig
  mcontent <- MemCache.get (fileid file) (AWS.fileCache ac)
  case mcontent of
      Just content -> return content
      Nothing -> do
        mcontentAWS <- AWS.getFileContents (AWS.mkAWSAction $ AWS.amazonConfig ac) file
        case mcontentAWS of
          Nothing -> do
            logInfo_ $ "Couldn't get content for file " ++ show (fileid file) ++ ", returning empty ByteString."
            return BS.empty
          Just contentAWS -> do
            MemCache.put (fileid file) contentAWS (AWS.fileCache ac)
            return contentAWS

getFileIDContents :: (MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, AWS.AmazonMonad m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  file <- dbQuery $ GetFileByFileID fid
  getFileContents file
