module FileStorage.Amazon
  ( mkAWSAction
  , uploadSomeFilesToAmazon
  , AmazonMonadT
  , runAmazonMonadT
  , exportFile
  , urlFromFile
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Time
import Log
import Network.AWS.Authentication
import System.FilePath ((</>))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.AWSResult as AWS
import qualified Network.AWS.S3Object as AWS
import qualified Network.HTTP as HTTP

import Crypto
import DB
import File.File
import File.Model
import FileStorage.Amazon.Config
import FileStorage.Class
import Log.Identifier

-- CORE-478: should be removed
mkAWSAction :: Maybe AmazonConfig -> AWS.S3Action
mkAWSAction mConfig =
  let emptyConfig = AmazonConfig
        { amazonConfigHost      = AWS.defaultAmazonS3Host
        , amazonConfigPort      = AWS.defaultAmazonS3Port
        , amazonConfigBucket    = ""
        , amazonConfigAccessKey = ""
        , amazonConfigSecretKey = ""
        }
      config = fromMaybe emptyConfig mConfig
  in s3ActionFromConfig config ""

newtype AmazonMonadT m a
  = AmazonMonadT { unAmazonMonadT :: ReaderT AmazonConfig m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b, MonadThrow
           , MonadCatch, MonadMask )

runAmazonMonadT :: Monad m => AmazonConfig -> AmazonMonadT m a -> m a
runAmazonMonadT config = flip runReaderT config . unAmazonMonadT

getAmazonConfig :: Monad m => AmazonMonadT m AmazonConfig
getAmazonConfig = AmazonMonadT ask

instance MonadTransControl AmazonMonadT where
  type StT AmazonMonadT m = StT (ReaderT AmazonConfig) m
  liftWith = defaultLiftWith AmazonMonadT unAmazonMonadT
  restoreT = defaultRestoreT AmazonMonadT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl IO m => MonadBaseControl IO (AmazonMonadT m) where
  type StM (AmazonMonadT m) a = ComposeSt AmazonMonadT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance {-# OVERLAPPING #-} (MonadBase IO m, MonadLog m, MonadThrow m)
    => MonadFileStorage (AmazonMonadT m) where
  saveNewContents     = saveNewContentsInAmazon
  getSavedContents    = getContentsFromAmazon
  deleteSavedContents = deleteContentsFromAmazon

saveNewContentsInAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m)
                    => String -> BSL.ByteString -> AmazonMonadT m ()
saveNewContentsInAmazon url contents = do
  config <- getAmazonConfig

  let conn = s3ConnFromConfig config
      obj  = (s3ObjectFromConfig config url) { AWS.obj_data = contents }
  result <- liftBase $ sendObjectMIC conn obj

  case result of
    Left err -> do
      logInfo "Saving file to AWS failed" $ object
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

-- CORE-478: should be removed
data GetContentRetry = RegularRetry
                     | RetryEncoded
                     | AnotherRetryEncoded
                     | NoRetry
  deriving Show

getContentsFromAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m)
                          => String -> AmazonMonadT m BSL.ByteString
getContentsFromAmazon = go RegularRetry
  where
    go :: (MonadBase IO m, MonadLog m, MonadThrow m) => GetContentRetry
       -> String -> AmazonMonadT m BSL.ByteString
    go retry url = do
      config <- getAmazonConfig
      let action = s3ActionFromConfig config url
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
            , "retry"        .= show retry
            ]
          case retry of
            RegularRetry -> go RetryEncoded url
            RetryEncoded -> go AnotherRetryEncoded $ HTTP.urlEncode url
            AnotherRetryEncoded -> go NoRetry url
            NoRetry -> throwM $ FileStorageException $ show err

deleteContentsFromAmazon :: (MonadBase IO m, MonadLog m, MonadThrow m) => String
                     -> AmazonMonadT m ()
deleteContentsFromAmazon url = do
  config <- getAmazonConfig

  let action = (s3ActionFromConfig config url) { AWS.s3operation = HTTP.DELETE }
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

-- CORE-478: should be removed
uploadSomeFilesToAmazon :: ( MonadBase IO m, MonadIO m, MonadLog m, MonadDB m
                           , MonadThrow m, CryptoRNG m )
                        => Maybe AmazonConfig -> Int -> m Bool
uploadSomeFilesToAmazon config n = do
  logInfo "Getting files to upload to AWS" $ object [
      "number_of_files" .= n
    ]
  startTime <- liftBase getCurrentTime
  let getDiffTime :: UTCTime -> Double
      getDiffTime ft = realToFrac $ diffUTCTime ft startTime
  files <- dbQuery $ GetFilesThatShouldBeMovedToAmazon n
  case files of
    [] -> do
      logInfo_ "No files to upload to AWS"
      return False
    _ -> do
      forM_ files $ \file -> do
        success <- exportFile (mkAWSAction config) file
        when (not success) $ do
          finishTime <- liftBase getCurrentTime
          logAttention "A file failed to upload to AWS" $ object [
              identifier_ (fileid file)
            , "elapsed_time" .= getDiffTime finishTime
            ]
          unexpectedError $ "Moving file " <+> show (fileid file) <+> " to Amazon failed."
      finishTime <- liftBase getCurrentTime
      logInfo "Finished uploading some files to AWS" $ object [
          "number_of_files" .= n
        , "elapsed_time" .= getDiffTime finishTime
        ]
      return True

-- CORE-478: should be moved
-- | Convert a file to Amazon URL. We use the following format:
--
-- > "file" </> fileid </> checksum </> filename
--
-- where filename and checksum will be urlencoded (percent encoded in UTF-8).
-- File name is preserved fully, that means you should supply file
-- extension already in place.
--
-- Note: Someday we might decide to publish temporarily externally
-- available links to files on Amazon. File names are already in
-- place, but Content-type is not, this will need to be fixed.
urlFromFile :: File -> String
urlFromFile File{filename, fileid, filechecksum} =
  -- here we use BSC.unpack, as HTTP.urlEncode
  -- does only %-escaping for 8bit values
  "file"
    </> show fileid
    </> (BSC.unpack . Base16.encode $ filechecksum)
    </> (HTTP.urlEncode . BSC.unpack . BS.fromString $ filename)

-- This is a fork of AWS.sendObjectMIC, that uses bytestrings for MD5
-- calculation, so it doesn't kill everything for 100mb objects
sendObjectMIC :: AWS.AWSConnection -> AWS.S3Object -> IO (AWS.AWSResult ())
sendObjectMIC aws obj = AWS.sendObject aws obj_w_header where
    obj_w_header = obj { AWS.obj_headers = (AWS.obj_headers obj) ++ md5_header }
    md5_header = [("Content-MD5", (mkMD5 (AWS.obj_data obj)))]
    mkMD5 = BSC.unpack . Base64.encode . MD5.hashlazy

-- CORE-478: should be removed
-- | Upload a document file. This means one of:
--
-- - upload a file to Amazon storage
-- - do nothing and keep it in memory database
exportFile :: (MonadBase IO m, MonadIO m, MonadDB m, MonadLog m, CryptoRNG m) => S3Action -> File -> m Bool
exportFile ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)}
           file@File{fileid, filestorage = FileStorageMemory plainContent} = localData [identifier_ fileid] $ do
  Right aes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
  let encryptedContent = aesEncrypt aes plainContent
      action = ctxs3action {
        AWS.s3object = url
      , AWS.s3operation = HTTP.PUT
      , AWS.s3body = BSL.fromChunks [encryptedContent]
      , AWS.s3metadata = []
      }
      url = urlFromFile file
      bucket = AWS.s3bucket ctxs3action
  (result, timeDiff) <- do
    startTime <- liftBase getCurrentTime
    result <- liftIO $ AWS.runAction action
    finishTime <- liftBase getCurrentTime
    return (result, realToFrac $ diffUTCTime finishTime startTime :: Double)
  case result of
    Right _ -> do
      _ <- dbUpdate $ FileMovedToAWS fileid url aes
      commit
      logInfo "AWS uploaded" $ object [
          "url" .= (bucket </> url)
        , "elapsed_time" .= timeDiff
        , identifier_ fileid
        ]
      return True
    Left err -> do
      logAttention "AWS failed to upload" $ object [
          "url" .= (bucket </> url)
        , "error" .= show err
        , "elapsed_time" .= timeDiff
        , identifier_ fileid
        ]
      return False

exportFile _ _ = do
  logInfo_ "No uploading to Amazon as bucket is ''"
  return False
