module File.Storage
  ( MonadFileStorage
  , saveNewFile
  , getFileContents
  , getFileIDContents
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Data.Time
import Log
import System.FilePath
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP as HTTP

import Crypto
import DB
import File.File
import File.Model
import FileStorage.Class (MonadFileStorage)
import Log.Identifier
import qualified FileStorage.Class as FS

-- | Create a new file, by uploading content straight to the underlying file
-- storage (S3 and caches.)
--
-- First creates an "empty" file using NewEmptyFileForAWS which has NULL
-- content, but other values set in database.
-- If upload succeeds, then it updates the file using FileMovedToAWS.
--
-- If the upload fails then the new NewEmptyFileForAWS is purged, and an
-- exception is thrown.
saveNewFile
  :: ( MonadBase IO m
     , MonadCatch m
     , MonadLog m
     , MonadDB m
     , MonadThrow m
     , CryptoRNG m
     , MonadFileStorage m
     )
  => Text
  -> BS.ByteString
  -> m FileID
saveNewFile fName fContent = do
  startTime       <- liftBase getCurrentTime
  (fid, checksum) <- dbUpdate $ NewEmptyFileForAWS fName fContent
  let awsUrl = urlFromFile fid fName checksum
  eaes <- mkAESConf <$> randomBytes 32 <*> randomBytes 16
  case eaes of
    Left err -> do
      logAttention "saveNewFile: failed to create AES config, purging file"
        $ object [identifier fid, "error" .= err]
      unexpectedError $ T.pack err
    Right aes -> localData [identifier fid] $ do
      let encryptedContent = aesEncrypt aes fContent
      eRes <- try . FS.saveNewContents awsUrl $ BSL.fromStrict encryptedContent
      case eRes of
        Right () -> do
          file       <- dbUpdate $ FileMovedToAWS fid awsUrl aes
          finishTime <- liftBase getCurrentTime
          logInfo "newFile: new file successfully created with content in S3" $ object
            [ logPair_ file
            , "elapsed_time" .= (realToFrac $ diffUTCTime finishTime startTime :: Double)
            ]
          return fid
        Left err@(FS.FileStorageException msg) -> do
          let attnMsg = "newFile: failed to upload to AWS, purging file"
          logAttention attnMsg $ object [identifier fid, "error" .= msg]
          dbUpdate $ PurgeFile fid
          logAttention "newFileInAmazon: purged file" $ object [identifier fid]
          throwM err

-- | Get file contents from the underlying storage and decrypt the contents
-- returning them as BS.
getFileContents
  :: (MonadFileStorage m, MonadBase IO m, MonadLog m, MonadThrow m)
  => File
  -> m BS.ByteString
getFileContents file@File { fileid, filestorage = FileStorageAWS url aes } =
  localData [identifier fileid] $ do
    encrypted <- FS.getSavedContents url
    let contents = aesDecrypt aes $ BSL.toStrict encrypted
        checksum = BA.convert $ H.hashWith H.SHA1 contents
    unless (checksum == filechecksum file) $ do
      logAttention "SHA1 checksums of file don't match" $ object [logPair_ file]
      throwM $ FS.FileStorageException
        "SHA1 checksum of file doesn't match the one in the database"
    return contents

getFileIDContents
  :: (MonadDB m, MonadFileStorage m, MonadBase IO m, MonadLog m, MonadThrow m)
  => FileID
  -> m BS.ByteString
getFileIDContents fid = getFileContents =<< dbQuery (GetFileByFileID fid)

-- | Build the Amazon URL for a file. We use the following format:
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
urlFromFile :: FileID -> Text -> BS.ByteString -> Text
urlFromFile fid name checksum =
  -- here we use BSC.unpack, as HTTP.urlEncode
  -- does only %-escaping for 8bit values
  T.pack
    $   "file"
    </> show fid
    </> (BS.unpack . Base16.encode $ checksum)
    </> (HTTP.urlEncode . BS.unpack . TE.encodeUtf8 $ name)
