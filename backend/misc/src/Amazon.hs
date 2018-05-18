{- All function related to Amazon Web Services -}
module Amazon (
    module Amazon.Class
  , isAWSConfigOk
  , mkAWSAction
  , uploadSomeFilesToAmazon
  , getFileFromRedis
  , getFileContents
  , AmazonMonadT
  , runAmazonMonadT
  , AmazonConfig(..)
  , deleteFile
  , exportFile
  , newFileInAmazon
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Time
import Log
import Network.AWS.Authentication
import System.FilePath ((</>))
import System.Timeout.Lifted
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Foldable as F
import qualified Database.Redis as R
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.AWSResult as AWS
import qualified Network.AWS.S3Object as AWS
import qualified Network.HTTP as HTTP

import Amazon.Class
import Crypto
import Database.Redis.Helpers
import DB
import File.File
import File.Model
import Log.Identifier

isAWSConfigOk :: Maybe (String, String, String) -> Bool
isAWSConfigOk (Just ((_:_), (_:_), (_:_))) = True
isAWSConfigOk _ = False

mkAWSAction :: Maybe (String, String, String) -> AWS.S3Action
mkAWSAction amazonConfig = AWS.S3Action {
    AWS.s3conn = AWS.amazonS3Connection accessKey secretKey
  , AWS.s3bucket = bucket
  , AWS.s3object = ""
  , AWS.s3query = ""
  , AWS.s3metadata = []
  , AWS.s3body = BSL.empty
  , AWS.s3operation = HTTP.GET
  }
  where
    (bucket, accessKey, secretKey) = fromMaybe ("","","") $ amazonConfig

newtype AmazonMonadT m a = AmazonMonadT { unAmazonMonadT :: ReaderT AmazonConfig m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadDB, MonadIO, MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b, MonadThrow, MonadCatch, MonadMask)

runAmazonMonadT :: Monad m => AmazonConfig -> AmazonMonadT m a -> m a
runAmazonMonadT ac = flip runReaderT ac . unAmazonMonadT

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

instance {-# OVERLAPPING #-} Monad m => AmazonMonad (AmazonMonadT m) where
  getAmazonConfig = AmazonMonadT ask

uploadSomeFilesToAmazon :: (AmazonMonad m, MonadBase IO m, MonadIO m,
                            MonadLog m, MonadDB m, MonadThrow m, CryptoRNG m)
                            => Int -> m Bool
uploadSomeFilesToAmazon n = do
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
      conf <- getAmazonConfig
      forM_ files $ \file -> do
        success <- exportFile (mkAWSAction $ awsConfig conf) file
        when (not success) $ do
          finishTime <- liftBase getCurrentTime
          logAttention "A file failed to upload to AWS" $ object [
              identifier_ (fileid file)
            , "elapsed_time" .= getDiffTime finishTime
            ]
          unexpectedError $ "Moving file" <+> show (fileid file)
            <+> "to Amazon failed."
      finishTime <- liftBase getCurrentTime
      logInfo "Finished uploading some files to AWS" $ object [
          "number_of_files" .= n
        , "elapsed_time" .= getDiffTime finishTime
        ]
      return True


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

-- | Create a new file, by uploading content straight to AWS S3, asking AWS to
-- perform an MD5 integrity check on the contents.
-- If AWS config is absent, database is used for storage.
--
-- First creates an "empty" file using NewEmptyFileForAWS which has NULL
-- content, but other values set in database.
-- If upload to AWS S3 succeeds, then it updates the file using FileMovedToAWS.
--
-- If the upload fails then the new NewEmptyFileForAWS is purged, and a
-- "regular" new file is created using NewFile.
newFileInAmazon :: (AmazonMonad m, MonadBase IO m, MonadIO m, MonadLog m,
                       MonadDB m, MonadThrow m, CryptoRNG m)
                      => String -> BS.ByteString -> m FileID
newFileInAmazon fName fContent = do
  mAwsConf <- awsConfig <$> getAmazonConfig
  case mAwsConf of
    Nothing -> do
      logInfo_ "newFileInAmazon: no AWS config, creating file in DB"
      dbUpdate $ NewFile fName fContent
    Just (awsBucket,awsAccessKey,awsSecretKey) -> do
      startTime <- liftBase getCurrentTime
      (fid, awsUrl) <- do
        emptyFile <- dbUpdate $ NewEmptyFileForAWS fName fContent
        return (fileid emptyFile, urlFromFile emptyFile)
      Right aes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
      let encryptedContent = aesEncrypt aes fContent
          s3Conn = AWS.amazonS3Connection awsAccessKey awsSecretKey
          s3Obj = AWS.S3Object { AWS.obj_bucket = awsBucket
                               , AWS.obj_name   = awsUrl
                               , AWS.content_type = ""
                               , AWS.obj_headers = []
                               , AWS.obj_data = BSL.fromChunks [encryptedContent]
                               }
      result <- liftIO $ sendObjectMIC s3Conn s3Obj
      case result of
        Right _ -> do
          dbUpdate $ FileMovedToAWS fid awsUrl aes
          finishTime <- liftBase getCurrentTime
          logInfo "newFileInAmazon: new file successfully created with content in S3" $ object [
              "url" .= (awsBucket </> awsUrl)
            , identifier_ fid
            , "elapsed_time" .= (realToFrac $ diffUTCTime finishTime startTime :: Double)
            ]
          return fid
        Left err -> do
          let attnMsg = "newFileInAmazon: failed to upload to AWS, purging file and creating new file in DB as fallback"
          logAttention attnMsg $ object [
              "url" .= (awsBucket </> awsUrl)
            , "error" .= show err
            , identifier_ fid
            ]
          dbUpdate $ PurgeFile fid
          fallback <- dbUpdate $ NewFile fName fContent
          logAttention "newFileInAmazon: purged file, new file created in DB as fallback" $
            object [ identifier_ fallback ]
          return fallback

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

deleteFile :: (MonadIO m, MonadDB m, MonadLog m) => S3Action -> String -> m Bool
deleteFile ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)} url = do
  let action = ctxs3action {
        AWS.s3object    = url
      , AWS.s3operation = HTTP.DELETE
      , AWS.s3metadata  = []
      }
  result <- liftIO $ AWS.runAction action
  case result of
    Right res -> do
      logInfo "AWS file deleted" $ object [
          "url" .= (AWS.s3bucket ctxs3action </> url)
        , "result" .= show res
        ]
      return True
    Left err -> do
      logAttention "AWS failed to delete file" $ object [
           "url" .= (AWS.s3bucket ctxs3action </> url)
         , "result" .= show err
         ]
      return False

deleteFile _ _ = do
  logInfo_ "No deleting file from Amazon as bucket is ''"
  return False

getFileFromRedis
  :: (MonadBaseControl IO m, MonadLog m)
  => R.Connection
  -> RedisKey
  -> m BS.ByteString
getFileFromRedis cache rkey = do
  semaphore <- newEmptyMVar
  withAsync (listener semaphore) $ \_ -> fix $ \loop -> do
    mcontent <- runRedis cache $ R.hget key "content"
    case mcontent of
      Just content -> do
        logInfo_ "File retrieved successfully"
        return content
      Nothing -> do
        logInfo_ "Waiting for file"
        void . timeout 1000000 $ takeMVar semaphore
        loop
  where
    key = fromRedisKey rkey

    listener semaphore = runRedis_ cache $ do
      R.pubSub (R.subscribe [key]) $ \_msg -> do
        void $ tryPutMVar semaphore ()
        return mempty

data GetContentRetry = RegularRetry
                     | RetryEncoded
                     | AnotherRetryEncoded
                     | NoRetry
  deriving Show

getFileContents
  :: forall m. (MonadBase IO m, MonadLog m, MonadThrow m)
  => S3Action
  -> File
  -> Maybe (R.Connection, RedisKey)
  -> m BS.ByteString
getFileContents s3action File{..} mredis = localData fileData $ do
  getContent RegularRetry filestorage >>= verifyContent >>= cacheContent
  where
    fileData = [
        identifier_ fileid
      , "filename" .= filename
      , "filesize" .= filesize
      ]

    getContent :: GetContentRetry -> FileStorage -> m BS.ByteString
    getContent _ (FileStorageMemory content) = return $ content
    getContent retry fs@(FileStorageAWS url aes) = do
      (result, timeDiff) <- do
        startTime <- liftBase getCurrentTime
        logInfo_ "Attempting to fetch file from AWS"
        result <- liftBase . AWS.runAction $ s3action {
            AWS.s3object = url
        }
        finishTime <- liftBase getCurrentTime
        return (result, realToFrac $ diffUTCTime finishTime startTime :: Double)
      case result of
        Right rsp -> do
          logInfo "Fetching file from AWS succeeded" $ object [
              "elapsed_time" .= timeDiff
            ]
          return . aesDecrypt aes . BSL.toStrict $ HTTP.rspBody rsp
        Left err -> do
          logAttention "Fetching file from AWS failed" $ object [
              "error" .= show err
            , "elapsed_time" .= timeDiff
            , "retry" .= show retry
            ]
          case retry of
            RegularRetry -> getContent RetryEncoded fs
            RetryEncoded -> getContent AnotherRetryEncoded $ FileStorageAWS (HTTP.urlEncode url) aes
            AnotherRetryEncoded -> getContent NoRetry fs
            NoRetry -> throwM $ AmazonException $ show err

    verifyContent :: BS.ByteString -> m BS.ByteString
    verifyContent content = if SHA1.hash content /= filechecksum
      then do
        logAttention_ "SHA1 checksum of file doesn't match the one in the database"
        throwM $ AmazonException $ "SHA1 checksum of file doesn't match the one in the database"
      else return content

    cacheContent :: BS.ByteString -> m BS.ByteString
    cacheContent content = do
      F.forM_ mredis $ redisPut "content" content
      return content
