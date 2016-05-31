{- All function related to Amazon Web Services -}
module Amazon (
    module Amazon.Class
  , isAWSConfigOk
  , mkAWSAction
  , uploadSomeFileToAmazon
  , getFileFromRedis
  , getFileContents
  , AmazonMonadT
  , runAmazonMonadT
  , AmazonConfig(..)
  , deleteFile
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Time
import Log
import Network.AWS.Authentication
import System.FilePath ((</>))
import System.Timeout.Lifted
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Foldable as F
import qualified Database.Redis as R
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.HTTP as HTTP

import Amazon.Class
import Crypto
import Crypto.RNG
import Database.Redis.Helpers
import DB
import File.File
import File.Model
import KontraPrelude
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

instance Monad m => AmazonMonad (AmazonMonadT m) where
  getAmazonConfig = AmazonMonadT ask

uploadSomeFileToAmazon :: (AmazonMonad m, MonadIO m, MonadLog m, MonadDB m, MonadThrow m, CryptoRNG m) => m Bool
uploadSomeFileToAmazon = do
  mfile <- dbQuery GetFileThatShouldBeMovedToAmazon
  case mfile of
    Nothing -> return False
    Just file -> do
      conf <- getAmazonConfig
      success <- exportFile (mkAWSAction $ awsConfig conf) file
      if success
        then return True
        else $unexpectedErrorM $ "Moving file " <+> show (fileid file) <+> " to Amazon failed."


-- | Convert a file to Amazon URL. We use the following format:
--
-- > "file" </> fileid </> filename
--
-- where filename will be urlencoded (percent encoded in UTF-8). File
-- name is preserved fully, that means you should supply file
-- extension already in place.
--
-- Note: Someday we might decide to publish temporarily externally
-- available links to files on Amazon. File names are already in
-- place, but Content-type is not, this will need to be fixed.
urlFromFile :: File -> String
urlFromFile File{filename, fileid} =
  -- here we use BSC.unpack, as HTTP.urlEncode
  -- does only %-escaping for 8bit values
  "file" </> show fileid </> (HTTP.urlEncode . BSC.unpack . BS.fromString $ filename)

-- | Upload a document file. This means one of:
--
-- - upload a file to Amazon storage
-- - do nothing and keep it in memory database
exportFile :: (MonadIO m, MonadDB m, MonadLog m, CryptoRNG m) => S3Action -> File -> m Bool
exportFile ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)}
           file@File{fileid, filestorage = FileStorageMemory plainContent} = localData [identifier_ fileid] $ do
  Right aes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
  let encryptedContent = aesEncrypt aes plainContent
  let action = ctxs3action {
        AWS.s3object = url
      , AWS.s3operation = HTTP.PUT
      , AWS.s3body = BSL.fromChunks [encryptedContent]
      , AWS.s3metadata = []
      }
      url = urlFromFile file
      bucket = AWS.s3bucket ctxs3action
  result <- liftIO $ AWS.runAction action
  case result of
    Right _ -> do
      logInfo "AWS uploaded" $ object [
          "url" .= (bucket </> url)
        ]
      _ <- dbUpdate $ FileMovedToAWS fileid bucket url aes
      return True
    Left err -> do
      logAttention "AWS failed to upload" $ object [
          "url" .= (bucket </> url)
        , "error" .= show err
        ]
      return False

exportFile _ _ = do
  logInfo_ "No uploading to Amazon as bucket is ''"
  return False

deleteFile :: (MonadIO m, MonadDB m, MonadLog m) => S3Action -> String -> String -> m Bool
deleteFile ctxs3action bucket url = do
  let action = ctxs3action {
        AWS.s3object    = url
      , AWS.s3operation = HTTP.DELETE
      , AWS.s3metadata  = []
      , AWS.s3bucket    = bucket
      }
  result <- liftIO $ AWS.runAction action
  case result of
    Right res -> do
      logInfo "AWS file deleted" $ object [
          "url" .= (bucket </> url)
        , "result" .= show res
        ]
      return True
    Left err -> do
      logAttention "AWS failed to delete file" $ object [
           "url" .= (bucket </> url)
         , "result" .= show err
         ]
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

getFileContents
  :: (MonadBase IO m, MonadLog m)
  => S3Action
  -> File
  -> Maybe (R.Connection, RedisKey)
  -> m (Maybe BS.ByteString)
getFileContents s3action File{..} mredis = localData fileData $ do
  mcontent <- getContent filestorage
  case mcontent of
    Nothing -> do
      logAttention_ "No content for file"
      return Nothing
    Just content -> do
      if isJust filechecksum && Just (SHA1.hash content) /= filechecksum
        then do
          logAttention_ "SHA1 checksum of file doesn't match the one in the database"
          return Nothing
        else do
          F.forM_ mredis $ redisPut "content" content
          return $ Just content
  where
    fileData = [
        identifier_ fileid
      , "filename" .= filename
      , "filesize" .= filesize
      ]

    getContent (FileStorageMemory content) = return . Just $ content
    getContent (FileStorageAWS bucket url aes) = do
      (result, timeDiff) <- do
        startTime <- liftBase getCurrentTime
        logInfo_ "Attempting to fetch file from AWS"
        result <- liftBase . AWS.runAction $ s3action {
            AWS.s3object = url
          , AWS.s3bucket = bucket
        }
        finishTime <- liftBase getCurrentTime
        return (result, realToFrac $ diffUTCTime finishTime startTime :: Double)
      case result of
        Right rsp -> do
          logInfo "Fetching file from AWS succeeded" $ object [
              "time" .= timeDiff
            ]
          return . Just . aesDecrypt aes . BSL.toStrict $ HTTP.rspBody rsp
        Left err -> do
          logAttention "Fetching file from AWS failed" $ object [
              "error" .= show err
            , "time" .= timeDiff
            ]
          return Nothing
