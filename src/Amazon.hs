{-
All function related to Amazon Web Services
-}

module Amazon (
      module Network.AWS.Authentication
    , urlFromFile
    , uploadFile
    , getFileContents
    ) where

--import Happstack.State (update)
import Network.AWS.Authentication
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import File.File
import File.Model
import Misc (concatChunks)
import qualified Log
import DB.Classes
import Control.Monad.IO.Class

-- | Convert a file to Amazon URL. We use the following format:
--
-- > "file" </> fileid </> filename ++ ".pdf"
--
-- where filename is urlencoded (percent encoded in utf-8)
urlFromFile :: File -> String
urlFromFile File{filename, fileid} =
    -- here we use BSC.unpack, as HTTP.urlEncode
    -- does only %-escaping for 8bit values
    "file" </> show fileid </> HTTP.urlEncode filename ++ ".pdf"

-- | Upload a document file. This means one of:
--
-- - upload a file to Amazon storage
-- - save a file in a local directory
-- - do nothing and keep it in memory database
uploadFile :: FilePath -> S3Action -> File -> DB ()
uploadFile docstore@(_:_) AWS.S3Action{AWS.s3bucket = ""} File{fileid, filename, filestorage = FileStorageMemory content} = do
    let filepath = docstore </> show fileid ++ '-' : filename ++ ".pdf"
    liftIO $ BS.writeFile filepath content
    Log.debug $ "Document file #" ++ show fileid ++ " saved as " ++ filepath
    dbUpdate $ FileMovedToDisk fileid filepath
    return ()

uploadFile _ ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)} file@File{fileid, filestorage = FileStorageMemory content} = do
    let action = ctxs3action { AWS.s3object = url
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
             return ()
         Left err -> do -- FIXME: do much better error handling
             Log.debug $ "AWS failed to upload of " ++ bucket </> url ++ " failed with error: " ++ show err
             return ()

uploadFile _ _ _ = do
    Log.debug "No uploading/saving to disk as bucket/docstore is ''"
    return ()

-- | Get file contents as strict 'BS.ByteString'. Either reads a file
-- from disk or memory or downloads from Amazon.
--
-- In case there are problems uses plain and old 'error'
-- function. Sorry for that.
--
-- FIXME: This function could use much better error reporting.
getFileContents :: S3Action -> File -> IO BS.ByteString
getFileContents _ File{filestorage = FileStorageDisk filepath} =
    BS.readFile filepath `catch` (\e -> do
        Log.debug $ show e
        return BS.empty)

getFileContents _ File{filestorage = FileStorageMemory content} =
    return content

getFileContents s3action File{filestorage = FileStorageAWS bucket url} = do
  let action = s3action { AWS.s3object = url
                        , AWS.s3bucket = bucket
                        }
  result <- AWS.runAction action
  case result of
       Right rsp -> return $ concatChunks $ HTTP.rspBody rsp
       _ -> error (show result)

