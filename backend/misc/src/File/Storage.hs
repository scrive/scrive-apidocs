module File.Storage
  ( MonadFileStorage
  , saveNewFile
  , getFileContents
  , getFileIDContents
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans
import Crypto.RNG
import Data.Time
import Log
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS

import Crypto
import DB
import File.File
import File.Model
import FileStorage.Class (MonadFileStorage)
import Log.Identifier
import qualified FileStorage.Amazon as A
import qualified FileStorage.Class as FS

-- | Create a new file, by uploading content straight to the underlying file
-- storage (S3 and caches.)
--
-- First creates an "empty" file using NewEmptyFileForAWS which has NULL
-- content, but other values set in database.
-- If upload succeeds, then it updates the file using FileMovedToAWS.
--
-- If the upload fails then the new NewEmptyFileForAWS is purged, and an
-- exception is thrown.
saveNewFile :: ( MonadBase IO m, MonadLog m, MonadDB m, MonadThrow m
               , CryptoRNG m, MonadFileStorage m )
            => String -> BS.ByteString -> m FileID
saveNewFile fName fContent = do
  startTime <- liftBase getCurrentTime
  emptyFile <- dbUpdate $ NewEmptyFileForAWS fName fContent
  let fid    = fileid emptyFile
      awsUrl = A.urlFromFile emptyFile
      -- CORE-478: urlFromFile should be moved in this module
  Right aes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
  let encryptedContent = aesEncrypt aes fContent
  mErr <- FS.saveNewFile awsUrl encryptedContent
  case mErr of
    Right () -> do
      dbUpdate $ FileMovedToAWS fid awsUrl aes
      -- CORE-478: Could be removed if NewEmptyFileForAWS were returning the final file
      file <- dbQuery $ GetFileByFileID fid
      finishTime <- liftBase getCurrentTime
      logInfo "newFile: new file successfully created with content in S3" $ object [
          logPair_ file
        , "elapsed_time" .= (realToFrac $ diffUTCTime finishTime startTime :: Double)
        ]
      return fid
    Left err -> do
      let attnMsg = "newFile: failed to upload to AWS, purging file and creating new file in DB as fallback"
      logAttention attnMsg $ object [
          logPair_ emptyFile
        , "error" .= err
        ]
      dbUpdate $ PurgeFile fid
      logAttention "newFileInAmazon: purged file" $ object [identifier_ fid]
      throwM $ A.AmazonException $ show err

-- | Gets file content from somewere (Amazon for now), putting it to cache and
-- returning as BS.
getFileContents :: (MonadFileStorage m, MonadIO m, MonadLog m, MonadThrow m)
                => File -> m BS.ByteString
getFileContents File{ filestorage = FileStorageMemory contents } =  return contents
getFileContents file@File{ filestorage = FileStorageAWS url aes } = do
  eEncrypted <- FS.getFileContents url
  case eEncrypted of
    Left err -> throwM $ A.AmazonException err
    Right encrypted -> do
      let contents = aesDecrypt aes encrypted
          checksum = SHA1.hash contents
      unless (checksum == filechecksum file) $ do
       logAttention "SHA1 checksums of file don't match" $ object
         [ "checksum" .= BS.unpack checksum
         , logPair_ file
         ]
       throwM $ A.AmazonException $
          "SHA1 checksum of file doesn't match the one in the database"
      return contents

getFileIDContents :: ( MonadDB m, MonadFileStorage m, MonadIO m, MonadLog m
                     , MonadThrow m ) => FileID -> m BS.ByteString
getFileIDContents fid = do
  start <- liftIO getCurrentTime
  file <- dbQuery $ GetFileByFileID fid
  result <- getFileContents file
  stop <- liftIO getCurrentTime
  logInfo "getFileIDContents timing" $ object
    [ "duration" .= realToFrac (diffUTCTime stop start :: Double) ]
  return result
