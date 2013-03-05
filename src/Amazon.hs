{- All function related to Amazon Web Services -}
module Amazon (
    isAWSConfigOk
  , mkAWSAction
  , uploadFilesToAmazon
  , getFileContents
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Maybe
import Network.AWS.Authentication
import Prelude hiding (catch)
import System.FilePath ((</>))
import qualified Network.AWS.AWSConnection as AWS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import AppConf
import ActionQueue.Scheduler
import Crypto
import DB
import File.File
import File.Model
import Utils.String
import qualified Log

isAWSConfigOk :: AppConf -> Bool
isAWSConfigOk conf = case amazonConfig conf of
  Just ((_:_), (_:_), (_:_)) -> True
  _ -> False

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
        then kCommit
        else do
          kRollback
          Log.debug "Uploading to Amazon failed, sleeping for 5 minutes."
          liftIO $ threadDelay $ 5 * 60 * 1000000
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
  Log.debug "No uploading to Amazon as bucket is ''"
  return False

getFileContents :: (MonadIO m, Log.MonadLog m) => S3Action -> File -> m (Maybe BS.ByteString)
getFileContents s3action File{..} = do
  mcontent <- liftIO $ getContent filestorage
  case mcontent of
    Nothing -> do
      Log.debug $ "No content for file " ++ show fileid
      return Nothing
    Just content -> do
      if isJust filechecksum && Just (SHA1.hash content) /= filechecksum
        then do
          Log.debug $ "CRITICAL: SHA1 checksum of file with id = " ++ show fileid ++ " doesn't match the one in the database"
          return Nothing
        else return $ Just content
  where
    getContent (FileStorageMemory content aes) = return . Just $ aesDecrypt aes content
    getContent (FileStorageAWS bucket url aes) = do
      result <- AWS.runAction $ s3action {
          AWS.s3object = url
        , AWS.s3bucket = bucket
      }
      case result of
        Right rsp -> return . Just . aesDecrypt aes . concatChunks $ HTTP.rspBody rsp
        Left err -> do
          Log.error $ "AWS.runAction failed with: " ++ show err
          return Nothing
