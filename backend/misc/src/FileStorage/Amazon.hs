module FileStorage.Amazon
  ( saveContentsToAmazon
  , getContentsFromAmazon
  , deleteContentsFromAmazon
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson.Types
import Log
import Optics (lensVL)
import qualified Conduit as C
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as AWS
import qualified Network.AWS.S3 as AWS
import qualified Network.HTTP as HTTP

import FileStorage.Amazon.S3Env
import FileStorage.Class
import Log.Utils

saveContentsToAmazon
  :: (MonadBase IO m, MonadLog m, MonadThrow m)
  => AmazonS3Env
  -> Text
  -> BSL.ByteString
  -> m ()
saveContentsToAmazon env url contents = go True =<< awsLogger
  where
    go retry logger = do
      logInfo "Attempting to save file to AWS" $ object ["url" .= url]
      (result, diff) <- timed . liftBase $ do
        withResourceT . runAWS env logger . AWS.send $ AWS.putObject
          (as3eBucket env)
          (urlObjectKey url)
          (AWS.toBody contents)
      case result of
        Right res -> do
          logInfo "File saved to AWS"
            $ object ["url" .= url, "result" .= show res, "time" .= diff]
        Left err -> do
          if retry
            then do
              logInfo "Saving file to AWS failed, retrying"
                $ object ["url" .= url, "error" .= show err, "time" .= diff]
              go False logger
            else do
              logAttention "Saving file to AWS failed"
                $ object ["url" .= url, "error" .= show err, "time" .= diff]
              throwM $ FileStorageException $ showt err

getContentsFromAmazon
  :: (MonadBase IO m, MonadLog m, MonadThrow m) => AmazonS3Env -> Text -> m BSL.ByteString
getContentsFromAmazon env url = go True =<< awsLogger
  where
    go retry logger = do
      logInfo "Attempting to fetch file from AWS" $ object ["url" .= url]
      (result, diff) <- timed . liftBase $ do
        withResourceT $ do
          rs <- runAWS env logger . AWS.send $ AWS.getObject (as3eBucket env)
                                                             (urlObjectKey url)
          C.runConduit $ AWS._streamBody (rs ^. lensVL AWS.gorsBody) C..| C.sinkLazy
      case result of
        Right rsp -> do
          logInfo "Fetching file from AWS succeeded"
            $ object ["url" .= url, "time" .= diff]
          return rsp
        Left err -> do
          if retry
            then do
              logInfo "Fetching file from AWS failed, retrying"
                $ object ["url" .= url, "error" .= show err, "time" .= diff]
              go False logger
            else do
              logAttention "Fetching file from AWS failed"
                $ object ["url" .= url, "error" .= show err, "time" .= diff]
              throwM $ FileStorageException $ showt err

deleteContentsFromAmazon
  :: (MonadBase IO m, MonadLog m, MonadThrow m) => AmazonS3Env -> Text -> m ()
deleteContentsFromAmazon env url = do
  logger <- awsLogger
  logInfo "Attempting to delete file from AWS" $ object ["url" .= url]
  (result, diff) <- timed . liftBase $ do
    withResourceT . runAWS env logger . AWS.send $ AWS.deleteObject (as3eBucket env)
                                                                    (urlObjectKey url)
  case result of
    Right res -> do
      logInfo "AWS file deleted"
        $ object ["url" .= url, "result" .= show res, "time" .= diff]
    Left err -> do
      logAttention "AWS failed to delete file"
        $ object ["url" .= url, "result" .= show err, "time" .= diff]
      throwM $ FileStorageException $ showt err

----------------------------------------

urlObjectKey :: Text -> AWS.ObjectKey
urlObjectKey = AWS.ObjectKey . T.pack . HTTP.urlDecode . T.unpack

withResourceT :: C.ResourceT IO r -> IO (Either AWS.Error r)
withResourceT = try . C.runResourceT

runAWS :: AmazonS3Env -> AWS.Logger -> AWS.AWS a -> C.ResourceT IO a
runAWS env lgr = AWS.runAWS (as3eEnv env & lensVL AWS.envLogger .~ lgr)

awsLogger :: MonadLog m => m AWS.Logger
awsLogger = do
  now    <- currentTime
  logger <- getLoggerIO
  return $ \awsLevel builder -> do
    let level = adjustLevel awsLevel
    -- We are not interested in trace messages.
    when (level /= LogTrace) $ logger now level (builderToText builder) emptyObject
  where
    adjustLevel AWS.Error = LogAttention
    adjustLevel AWS.Info  = LogInfo
    adjustLevel AWS.Debug = LogInfo
    adjustLevel AWS.Trace = LogTrace

    builderToText =
      either (T.append "decode error: " . showt) identity
        . T.decodeUtf8'
        . BSL.toStrict
        . B.toLazyByteString
