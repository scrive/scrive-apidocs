{-

All function related to Amazon Web Services

-}

module Amazon 
    ( module Network.AWS.Authentication
    , urlFromFile
    , uploadFile
    , getFileContents
    )
where

import Doc.DocState
import Network.AWS.Authentication
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import Misc (concatChunks)
import Happstack.State (update)
import System.FilePath ((</>))

urlFromFile :: File -> String
urlFromFile File{filename,fileid} =
    -- here we use BSC.unpack, as HTTP.urlEncode
    -- does only %-escaping for 8bit values
    "file/" ++ show fileid ++ "/" ++ HTTP.urlEncode (BSC.unpack filename) ++ ".pdf"

uploadFile :: FilePath -> S3Action -> File -> IO ()
uploadFile docstore@(_:_) ctxs3action@AWS.S3Action{ AWS.s3bucket = "" } File{ fileid, filename, filestorage = FileStorageMemory content } = do
  let filepath = docstore </> show fileid ++ '-' : BSC.unpack filename ++ ".pdf"
  BS.writeFile filepath content
  putStrLn $ "Document saved as " ++ filepath
  update $ FileMovedToDisk fileid filepath
  return ()

uploadFile _ ctxs3action@AWS.S3Action{ AWS.s3bucket = (_:_) } file@File{fileid, filestorage = FileStorageMemory content} = do
  let action = ctxs3action { AWS.s3object = url
                           , AWS.s3operation = HTTP.PUT
                           , AWS.s3body = BSL.fromChunks [content]
                           , AWS.s3metadata = [("Content-Type","application/pdf")]
                           }
      url = urlFromFile file
      bucket = AWS.s3bucket ctxs3action
  result <- AWS.runAction action
  case result of
    Right _ -> do
                putStrLn $ "AWS uploaded: " ++ bucket ++ "/" ++ url
                update $ FileMovedToAWS fileid (BS.fromString bucket) (BS.fromString url)
                return ()


    -- FIXME: do much better error handling
    Left err -> do
                putStrLn $ "AWS failed to upload: " ++ bucket ++ "/" ++ url
                print err 
                return ()

uploadFile _ _ _ = do
    putStrLn "No uploading/saving to disk as bucket/docstore is ''"
    return ()


getFileContents :: S3Action -> File -> IO BS.ByteString
getFileContents _ File{ filestorage = FileStorageDisk filepath } =
    BS.readFile filepath `catch` (return . const BS.empty)

getFileContents s3action File{ filestorage = FileStorageMemory content, filename } = do
    -- putStrLn $ "getFileContents local " ++ BS.toString filename
    return content
getFileContents s3action File{ filestorage = FileStorageAWS bucket url } = do
  -- putStrLn $ "AWS download " ++ BS.toString bucket ++ "/" ++ BS.toString url
  result <- AWS.runAction (s3action { AWS.s3object = BS.toString url
                                       , AWS.s3bucket = BS.toString bucket
                                       })
  case result of
    Right rsp -> return (concatChunks (HTTP.rspBody rsp))
    _ -> error (show result)
