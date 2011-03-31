{-# OPTIONS_GHC -Wall -Werror #-}

{-
All function related to Amazon Web Services
-}

module Amazon (
      module Network.AWS.Authentication
    , urlFromFile
    , uploadFile
    , getFileContents
    ) where

import Happstack.State (update)
import Network.AWS.Authentication
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import Doc.DocState
import Misc (concatChunks)


urlFromFile :: File -> String
urlFromFile File{filename, fileid} =
    -- here we use BSC.unpack, as HTTP.urlEncode
    -- does only %-escaping for 8bit values
    "file/" ++ show fileid ++ "/" ++ HTTP.urlEncode (BSC.unpack filename) ++ ".pdf"

uploadFile :: FilePath -> S3Action -> File -> IO ()
uploadFile docstore@(_:_) AWS.S3Action{AWS.s3bucket = ""} File{fileid, filename, filestorage = FileStorageMemory content} = do
    let filepath = docstore </> show fileid ++ '-' : BSC.unpack filename ++ ".pdf"
    BS.writeFile filepath content
    putStrLn $ "Document saved as " ++ filepath
    _ <- update $ FileMovedToDisk fileid filepath
    return ()

uploadFile _ ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)} file@File{fileid, filestorage = FileStorageMemory content} = do
    let action = ctxs3action {
          AWS.s3object = url
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
             _ <- update $ FileMovedToAWS fileid (BS.fromString bucket) (BS.fromString url)
             return ()
         Left err -> do -- FIXME: do much better error handling
             putStrLn $ "AWS failed to upload: " ++ bucket ++ "/" ++ url
             print err 
             return ()

uploadFile _ _ _ = do
    putStrLn "No uploading/saving to disk as bucket/docstore is ''"
    return ()

getFileContents :: S3Action -> File -> IO BS.ByteString
getFileContents _ File{filestorage = FileStorageDisk filepath} =
    BS.readFile filepath `catch` (\e -> do
        putStrLn $ show e
        return BS.empty)

getFileContents _ File{filestorage = FileStorageMemory content} =
    return content

getFileContents s3action File{filestorage = FileStorageAWS bucket url} = do
  -- putStrLn $ "AWS download " ++ BS.toString bucket ++ "/" ++ BS.toString url
  let action = s3action {
        AWS.s3object = BS.toString url
      , AWS.s3bucket = BS.toString bucket
  }
  result <- AWS.runAction action
  case result of
       Right rsp -> return $ concatChunks $ HTTP.rspBody rsp
       _ -> error (show result)
