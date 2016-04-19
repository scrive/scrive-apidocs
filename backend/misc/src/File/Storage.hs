module File.Storage (
    getFileContents
  , getFileIDContents
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Log
import qualified Data.ByteString as BS

import DB
import File.File
import File.Model
import KontraPrelude
import Log.Identifier
import qualified Amazon as AWS
import qualified MemCache as MemCache

{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: (MonadLog m, MonadBaseControl IO m, AWS.AmazonMonad m) => File -> m BS.ByteString
getFileContents file = do
  ac <- AWS.getAmazonConfig
  MemCache.fetch (AWS.fileCache ac) (fileid file) $ do
    mcontent <- AWS.getFileContents (AWS.mkAWSAction $ AWS.amazonConfig ac) file
    case mcontent of
      Just content -> return content
      Nothing -> do
        logAttention "Couldn't get content for file, returning empty ByteString." $ object [
            identifier_ $ fileid file
          ]
        return BS.empty

getFileIDContents :: (MonadDB m, MonadThrow m, MonadLog m, MonadBaseControl IO m, AWS.AmazonMonad m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  file <- dbQuery $ GetFileByFileID fid
  getFileContents file
