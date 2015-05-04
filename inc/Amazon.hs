{- All function related to Amazon Web Services -}
module Amazon (
    module Amazon.Class
  , isAWSConfigOk
  , mkAWSAction
  , uploadSomeFileToAmazon
  , getFileContents
  , AmazonMonadT
  , runAmazonMonadT
  , AmazonConfig(..)
  , deleteFile
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Network.AWS.Authentication
import System.FilePath ((</>))
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.HTTP as HTTP

import Amazon.Class
import Crypto
import Crypto.RNG
import DB
import File.File
import File.Model
import KontraPrelude
import Log
import Utils.String

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

instance Monad m => AmazonMonad (AmazonMonadT m) where
  getAmazonConfig = AmazonMonadT ask

uploadSomeFileToAmazon :: (AmazonMonad m, MonadIO m, MonadLog m, MonadDB m, MonadThrow m, CryptoRNG m) => m Bool
uploadSomeFileToAmazon = do
  mfile <- dbQuery GetFileThatShouldBeMovedToAmazon
  case mfile of
    Nothing -> return False
    Just file -> do
      conf <- getAmazonConfig
      success <- exportFile (mkAWSAction $ amazonConfig conf) file
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
      logInfo "AWS uploaded" $ object [
          "fileid" .= show fileid
        , "url" .= (bucket </> url)
        ]
      _ <- dbUpdate $ FileMovedToAWS fileid bucket url aes
      return True
    Left err -> do
      logAttention "AWS failed to upload" $ object [
          "fileid" .= show fileid
        , "url" .= (bucket </> url)
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


getFileContents :: (MonadBase IO m, MonadLog m) => S3Action -> File -> m (Maybe BS.ByteString)
getFileContents s3action File{..} = do
  mcontent <- getContent filestorage
  case mcontent of
    Nothing -> do
      logAttention "No content for file" $ object [
          "fileid" .= show fileid
        , "filename" .= filename
        ]
      return Nothing
    Just content -> do
      if isJust filechecksum && Just (SHA1.hash content) /= filechecksum
        then do
          logAttention "SHA1 checksum of file doesn't match the one in the database" $ object [
              "fileid" .= show fileid
            , "filename" .= filename
            ]
             -- value "database_sha1" filechecksum
             -- value "calculated_sha1" (SHA1.hash content)
          return Nothing
        else return $ Just content
  where
    getContent (FileStorageMemory content) = return . Just $ content
    getContent (FileStorageAWS bucket url aes) = do
      result <- liftBase $ AWS.runAction $ s3action {
          AWS.s3object = url
        , AWS.s3bucket = bucket
      }
      case result of
        Right rsp -> return . Just . aesDecrypt aes . concatChunks $ HTTP.rspBody rsp
        Left err -> do
          logAttention "AWS.runAction failed"  $ object [
              "fileid" .= show fileid
            , "error" .= show err
            , "filename" .= filename
            ]
          return Nothing
