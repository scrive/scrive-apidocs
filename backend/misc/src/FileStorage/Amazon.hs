module FileStorage.Amazon
  ( saveContentsToAmazon
  , getContentsFromAmazon
  , deleteContentsFromAmazon
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Time
import Log
import System.FilePath ((</>))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.AWSResult as AWS
import qualified Network.AWS.S3Object as AWS
import qualified Network.HTTP as HTTP

import FileStorage.Amazon.Config
import FileStorage.Class

saveContentsToAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m)
                     => AmazonConfig -> String -> BSL.ByteString -> m ()
saveContentsToAmazon config url contents = do
    let conn = s3ConnFromConfig config
        obj  = (s3ObjectFromConfig config url) { AWS.obj_data = contents }

    go True conn obj

  where
    go retry conn obj = do
      result <- liftBase $ sendObjectMIC conn obj
      case result of
        Left err -> do
          if retry
            then do
              logInfo "Saving file to AWS failed, retrying" $ object
                [ "url"    .= url
                , "result" .= show err
                ]
              go False conn obj
            else do
              logAttention "Saving file to AWS failed" $ object
                [ "url"    .= url
                , "result" .= show err
                ]
              throwM $ FileStorageException $ show err
        Right res -> do
          logInfo "Filed saved on AWS" $ object
            [ "url"    .= url
            , "result" .= show res
            ]
          return ()

    -- This is a fork of AWS.sendObjectMIC, that uses bytestrings for MD5
    -- calculation, so it doesn't kill everything for 100mb objects
    sendObjectMIC :: AWS.AWSConnection -> AWS.S3Object -> IO (AWS.AWSResult ())
    sendObjectMIC aws obj = AWS.sendObject aws obj_w_header where
      obj_w_header = obj { AWS.obj_headers = (AWS.obj_headers obj) ++ md5_header }
      md5_header = [("Content-MD5", (mkMD5 (AWS.obj_data obj)))]
      mkMD5 = BSC.unpack . Base64.encode . MD5.hashlazy

getContentsFromAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m)
                      => AmazonConfig -> String -> m BSL.ByteString
getContentsFromAmazon config url = do
  let action = s3ActionFromConfig config HTTP.GET url
  logInfo "Attempting to fetch file from AWS" $ object ["url" .= url]

  startTime  <- liftBase getCurrentTime
  result     <- liftBase $ AWS.runAction action
  finishTime <- liftBase getCurrentTime
  let diff = realToFrac $ diffUTCTime finishTime startTime :: Double

  case result of
    Right rsp -> do
      logInfo "Fetching file from AWS succeeded" $ object
        [ "elapsed_time" .= diff
        , "url"          .= url
        ]
      return $ HTTP.rspBody rsp
    Left err -> do
      logAttention "Fetching file from AWS failed" $ object
        [ "error"        .= show err
        , "elapsed_time" .= diff
        , "url"          .= url
        ]
      throwM $ FileStorageException $ show err

deleteContentsFromAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m)
                         => AmazonConfig -> String -> m ()
deleteContentsFromAmazon config url = do
  let action = s3ActionFromConfig config HTTP.DELETE url
  result <- liftBase $ AWS.runAction action
  case result of
    Right res -> do
      logInfo "AWS file deleted" $ object
        [ "url"    .= (AWS.s3bucket action </> url)
        , "result" .= show res
        ]
      return ()
    Left err -> do
      logAttention "AWS failed to delete file" $ object
         [ "url"    .= (AWS.s3bucket action </> url)
         , "result" .= show err
         ]
      throwM $ FileStorageException $ show err
