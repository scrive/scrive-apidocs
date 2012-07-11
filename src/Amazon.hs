{- All function related to Amazon Web Services -}
module Amazon (
    mkAWSAction
  , uploadFilesToAmazon
  , getFileContents
  , calculateChecksumAndEncryptOldFiles
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import Data.Maybe
import Network.AWS.Authentication
import Prelude hiding (catch)
import System.FilePath ((</>))
import qualified Network.AWS.AWSConnection as AWS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import AppConf
import ActionQueue.Scheduler
import Crypto
import DB
import File.File
import File.Model
import Misc (concatChunks)
import qualified Log

mkAWSAction :: AppConf -> AWS.S3Action
mkAWSAction appConf = AWS.S3Action {
    AWS.s3conn = AWS.amazonS3Connection accessKey secretKey
  , AWS.s3bucket = bucket
  , AWS.s3object = ""
  , AWS.s3query = ""
  , AWS.s3metadata = []
  , AWS.s3body = BSL.empty
  , AWS.s3operation = HTTP.GET
  }
  where
    (bucket, accessKey, secretKey) = fromMaybe ("","","") $ amazonConfig appConf

uploadFilesToAmazon :: Scheduler ()
uploadFilesToAmazon = do
  mfile <- dbQuery GetFileThatShouldBeMovedToAmazon
  case mfile of
    Nothing -> return ()
    Just file -> do
      conf <- sdAppConf <$> ask
      success <- exportFile (mkAWSAction conf) file
      if success
        then dbCommit
        else dbRollback
      uploadFilesToAmazon

-- | Transition function between non-encrypted and encrypted files.
-- To be removed after 15.08.2012.
calculateChecksumAndEncryptOldFiles :: Scheduler ()
calculateChecksumAndEncryptOldFiles = do
  mfile <- dbQuery GetFileWithNoChecksum
  case mfile of
    Nothing -> Log.debug "Encrypting old files done."
    Just file -> do
      let fid = fileid file
      Log.debug $ "Generating checksum, encrypting and saving in the database file with id = " ++ show fid ++ "..."
      conf <- sdAppConf <$> ask
      content <- Binary <$> getFileContents (mkAWSAction conf) file
      op1 <- dbUpdate $ SetChecksum fid $ SHA1.hash `binApp` content
      op2 <- dbUpdate $ SetContentToMemoryAndEncryptIt fid content
      case (unBinary content /= BS.empty, op1, op2) of
        (True, True, True) -> do
          Log.debug $ "Operation succeeded for file " ++ show fid ++ "."
          dbCommit
          liftIO $ threadDelay 500000 -- 0.5 sec
          calculateChecksumAndEncryptOldFiles
        res -> do
          Log.debug $ "Operation failed for file " ++ show fid ++ " - (content not empty, op1, op2) = " ++ show res ++ ", aborting."
          dbRollback

-- | Convert a file to Amazon URL. We use the following format:
--
-- > "file" </> fileid </> filename ++ ".pdf"
--
-- where filename is urlencoded (percent encoded in utf-8)
urlFromFile :: File -> String
urlFromFile File{filename, fileid} =
  -- here we use BSC.unpack, as HTTP.urlEncode
  -- does only %-escaping for 8bit values
  "file" </> show fileid </> (HTTP.urlEncode . BSC.unpack . BS.fromString $ filename) ++ ".pdf"

-- | Upload a document file. This means one of:
--
-- - upload a file to Amazon storage
-- - do nothing and keep it in memory database
exportFile :: S3Action -> File -> Scheduler Bool
exportFile ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)} file@File{fileid, filestorage = FileStorageMemory content _} = do
  let action = ctxs3action {
        AWS.s3object = url
      , AWS.s3operation = HTTP.PUT
      , AWS.s3body = BSL.fromChunks [content]
      , AWS.s3metadata = [("Content-Type","application/pdf")]
      }
      url = urlFromFile file
      bucket = AWS.s3bucket ctxs3action
  result <- liftIO $ AWS.runAction action
  case result of
    Right _ -> do
      Log.debug $ "AWS uploaded " ++ bucket </> url
      _ <- dbUpdate $ FileMovedToAWS fileid bucket url
      return True
    Left err -> do -- FIXME: do much better error handling
      Log.debug $ "AWS failed to upload of " ++ bucket </> url ++ " failed with error: " ++ show err
      return False

exportFile _ _ = do
  Log.debug "No uploading/saving to disk as bucket/docstore is ''"
  return True

getFileContents :: MonadIO m => S3Action -> File -> m BS.ByteString
getFileContents s3action File{..} = do
  content <- liftIO $ getContent filestorage
  if isJust filechecksum && Just (SHA1.hash content) /= filechecksum
     then do
       Log.debug $ "CRITICAL: SHA1 checksum of file with id = " ++ show fileid ++ " doesn't match the one in the database"
       return BS.empty
     else return content
  where
    getContent (FileStorageDisk filepath) = do
      BS.readFile filepath `catch` (\(e :: SomeException) -> do
        Log.debug $ "Reading file " ++ filepath ++ " failed with: " ++ show e
        return BS.empty)
    getContent (FileStorageMemory content aes) = return $ aesDecrypt aes content
    getContent (FileStorageAWS bucket url aes) = do
      result <- AWS.runAction $ s3action {
          AWS.s3object = url
        , AWS.s3bucket = bucket
      }
      case result of
        Right rsp -> return . maybe id aesDecrypt aes . concatChunks $ HTTP.rspBody rsp
        Left err -> do
          Log.error $ "AWS.runAction failed with: " ++ show err
          return BS.empty
