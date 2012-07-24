

module File.Storage
    ( getFileContents
    , getFileIDContents
    ) where

import Control.Monad.Trans
import DB
import qualified Amazon as AWS
import Kontra
import File.Model
import File.File
import qualified Data.ByteString as BS
import qualified MemCache as MemCache



{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: (KontraMonad m, MonadIO m) => File -> m BS.ByteString
getFileContents file = do
  ctx <- getContext
  mcontent <- MemCache.get (fileid file) (ctxfilecache ctx)
  case mcontent of
      Just content -> return content
      Nothing -> do
        mcontentAWS <- liftIO $ AWS.getFileContents (ctxs3action ctx) file
        MemCache.put (fileid file) mcontentAWS (ctxfilecache ctx)
        return mcontentAWS

getFileIDContents :: (KontraMonad m, MonadDB m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  mfile <- dbQuery $ GetFileByFileID fid
  case mfile of
    Just file -> getFileContents file
    Nothing -> return BS.empty
