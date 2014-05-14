{- All function related to Amazon Web Services -}
module Amazon (
    isAWSConfigOk
  , mkAWSAction
  , uploadFilesToAmazon
  , getFileContents
  , AmazonMonadT
  , runAmazonMonadT
  , AmazonConfig(..)
  , AmazonMonad(..)
  , deleteFile
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.Monad.Reader
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import Data.Maybe
import Network.AWS.Authentication
import qualified MemCache
import System.FilePath ((</>))
import Happstack.Server hiding (result)
import qualified Network.AWS.AWSConnection as AWS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import Text.JSON.Gen

import Crypto
import Crypto.RNG
import DB
import File.File
import File.Model
import Utils.String
import qualified Log

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

data AmazonConfig = AmazonConfig { amazonConfig :: Maybe (String, String, String)
                                 , fileCache    :: MemCache.MemCache FileID BS.ByteString
                                 }

newtype AmazonMonadT m a = AmazonMonadT { unAmazonMonadT :: ReaderT AmazonConfig m a }
    deriving (Alternative, Applicative, Functor, Monad, MonadDB, MonadIO, Log.MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b)

instance MonadTransControl AmazonMonadT where
  newtype StT AmazonMonadT m = StAmazonMonadT { unStAmazonMonadT :: StT (ReaderT AmazonConfig) m }
  liftWith = defaultLiftWith AmazonMonadT unAmazonMonadT StAmazonMonadT
  restoreT = defaultRestoreT AmazonMonadT unStAmazonMonadT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}


instance MonadBaseControl IO m => MonadBaseControl IO (AmazonMonadT m) where
  newtype StM (AmazonMonadT m) a = StMAmazonMonadT { unStMAmazonMonadT :: ComposeSt AmazonMonadT m a }
  liftBaseWith = defaultLiftBaseWith StMAmazonMonadT
  restoreM     = defaultRestoreM unStMAmazonMonadT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance WebMonad Response m => WebMonad Response (AmazonMonadT m) where
    finishWith = AmazonMonadT . finishWith

instance ServerMonad m => ServerMonad (AmazonMonadT m) where
    askRq = lift askRq
    localRq f m = AmazonMonadT $ localRq f $ unAmazonMonadT m

instance (Monad m, HasRqData m) => HasRqData (AmazonMonadT m) where
    askRqEnv = lift askRqEnv
    localRqEnv f m = AmazonMonadT $ localRqEnv f $ unAmazonMonadT m
    rqDataError = AmazonMonadT . rqDataError

instance FilterMonad Response m => FilterMonad Response (AmazonMonadT m) where
    setFilter = AmazonMonadT . setFilter
    getFilter = AmazonMonadT . getFilter . unAmazonMonadT
    composeFilter = AmazonMonadT . composeFilter

instance (Monad m, AmazonMonad m) => AmazonMonad (ReaderT r m) where
    getAmazonConfig = lift getAmazonConfig

instance (Monad m, AmazonMonad m) => AmazonMonad (Strict.StateT s m) where
    getAmazonConfig = lift getAmazonConfig

instance (Monad m, AmazonMonad m) => AmazonMonad (Lazy.StateT s m) where
    getAmazonConfig = lift getAmazonConfig

runAmazonMonadT :: Monad m => AmazonConfig -> AmazonMonadT m a -> m a
runAmazonMonadT ac = flip runReaderT ac . unAmazonMonadT

class AmazonMonad m where
    getAmazonConfig :: m AmazonConfig

instance Monad m => AmazonMonad (AmazonMonadT m) where
    getAmazonConfig = AmazonMonadT $ ReaderT return

uploadFilesToAmazon :: (AmazonMonad m, MonadIO m, Log.MonadLog m, MonadDB m, CryptoRNG m) => m ()
uploadFilesToAmazon = do
  mfile <- dbQuery GetFileThatShouldBeMovedToAmazon
  case mfile of
    Nothing -> return ()
    Just file -> do
      conf <- getAmazonConfig
      success <- exportFile (mkAWSAction $ amazonConfig conf) file
      if success
        then commit
        else do
          rollback
          Log.attention_ "Uploading to Amazon failed, sleeping for 5 minutes."
          liftIO $ threadDelay $ 5 * 60 * 1000000
      uploadFilesToAmazon

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
exportFile :: (MonadIO m, MonadDB m, Log.MonadLog m, CryptoRNG m) => S3Action -> File -> m Bool
exportFile ctxs3action@AWS.S3Action{AWS.s3bucket = (_:_)}
           file@File{fileid, filestorage = FileStorageMemory plainContent} = do
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
      Log.mixlog "AWS uploaded" $ do
          value "fileid" (show fileid)
          value "url" (bucket </> url)
      _ <- dbUpdate $ FileMovedToAWS fileid bucket url aes
      return True
    Left err -> do
      Log.attention "AWS failed to upload" $ do
          value "fileid" (show fileid)
          value "url" (bucket </> url)
          value "error" (show err)
      return False

exportFile _ _ = do
  Log.mixlog_ "No uploading to Amazon as bucket is ''"
  return False

deleteFile :: (MonadIO m, MonadDB m, Log.MonadLog m) => S3Action -> String -> String -> m Bool
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
      Log.mixlog "AWS file deleted" $ do
         value "url" $ bucket </> url
         value "result" (show res)
      return True
    Left err -> do
      Log.attention "AWS failed to delete file" $ do
         value "url" $ bucket </> url
         value "result" (show err)
      return False


getFileContents :: (MonadIO m, Log.MonadLog m) => S3Action -> File -> m (Maybe BS.ByteString)
getFileContents s3action File{..} = do
  mcontent <- getContent filestorage
  case mcontent of
    Nothing -> do
      Log.attention "No content for file" $ do
         value "fileid" (show fileid)
         value "filename" filename
      return Nothing
    Just content -> do
      if isJust filechecksum && Just (SHA1.hash content) /= filechecksum
        then do
          Log.attention "SHA1 checksum of file doesn't match the one in the database" $ do
             value "fileid" (show fileid)
             value "filename" filename
             -- value "database_sha1" filechecksum
             -- value "calculated_sha1" (SHA1.hash content)
          return Nothing
        else return $ Just content
  where
    getContent (FileStorageMemory content) = return . Just $ content
    getContent (FileStorageAWS bucket url aes) = do
      result <- liftIO $ AWS.runAction $ s3action {
          AWS.s3object = url
        , AWS.s3bucket = bucket
      }
      case result of
        Right rsp -> return . Just . aesDecrypt aes . concatChunks $ HTTP.rspBody rsp
        Left err -> do
          Log.mixlog "AWS.runAction failed" $ do
             value "fileid" (show fileid)
             value "error" (show err)
             value "filename" filename
          return Nothing
