{- All function related to Amazon Web Services -}
module Amazon (
    uploadFilesToAmazon
  , getFileContents
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import Network.AWS.Authentication
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import Prelude hiding (catch)

import AppConf
import AppControl
import ActionQueue.Scheduler
import Crypto
import Crypto.RNG
import DB
import File.File
import File.Model
import Misc (concatChunks)
import qualified Log
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

uploadFilesToAmazon :: Scheduler ()
uploadFilesToAmazon = do
  mfile <- dbQuery GetFileThatShouldBeMovedToAmazon
  case mfile of
    Nothing -> return ()
    Just file -> do
      conf <- sdAppConf `fmap` ask
      success <- uploadFile (docstore conf) (defaultAWSAction conf) file
      if success
        then dbCommit
        else dbRollback
      uploadFilesToAmazon

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
-- - save a file in a local directory
-- - do nothing and keep it in memory database
uploadFile :: FilePath -> S3Action -> File -> Scheduler Bool
uploadFile docstore@(_:_) AWS.S3Action{AWS.s3bucket = ""} File{fileid, filename, filestorage = FileStorageMemory content} = do
    let filepath = docstore </> show fileid ++ '-' : filename ++ ".pdf"
    liftIO $ BS.writeFile filepath content
    Log.debug $ "Document file #" ++ show fileid ++ " saved as " ++ filepath
    dbUpdate $ FileMovedToDisk fileid filepath
    return True

uploadFile _ ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)} file@File{fileid, filestorage = FileStorageMemory content} = do
    Right aes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
    let action = ctxs3action { AWS.s3object = url
                             , AWS.s3operation = HTTP.PUT
                             , AWS.s3body = BSL.fromChunks [aesEncrypt aes content]
                             , AWS.s3metadata = [("Content-Type","application/pdf")]
                             }
        url = urlFromFile file
        bucket = AWS.s3bucket ctxs3action
    result <- liftIO $ AWS.runAction action
    case result of
         Right _ -> do
             Log.debug $ "AWS uploaded " ++ bucket </> url
             _ <- dbUpdate $ FileMovedToAWS fileid bucket url aes
             return True
         Left err -> do -- FIXME: do much better error handling
             Log.debug $ "AWS failed to upload of " ++ bucket </> url ++ " failed with error: " ++ show err
             return False

uploadFile _ _ _ = do
    Log.debug "No uploading/saving to disk as bucket/docstore is ''"
    return True

-- | Get file contents as strict 'BS.ByteString'. Either reads a file
-- from disk or memory or downloads from Amazon.
--
-- In case there are problems uses plain and old 'error'
-- function. Sorry for that.
--
-- FIXME: This function could use much better error reporting.
getFileContents :: S3Action -> File -> IO BS.ByteString
getFileContents _ File{filestorage = FileStorageDisk filepath} =
    BS.readFile filepath `catch` (\(e :: SomeException) -> do
        Log.debug $ show e
        return BS.empty)

getFileContents _ File{filestorage = FileStorageMemory content} =
    return content

getFileContents s3action File{filestorage = FileStorageAWS bucket url aes} = do
  let action = s3action { AWS.s3object = url
                        , AWS.s3bucket = bucket
                        }
  result <- AWS.runAction action
  case result of
       Right rsp -> return . aesDecrypt aes . concatChunks $ HTTP.rspBody rsp
       _ -> error (show result)
