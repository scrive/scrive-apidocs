module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetMaybeFileByFileID(..)
    , NewEmptyFileForAWS(..)
    , PurgeFile(..)
    ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Crypto
import DB
import File.Conditions
import File.File
import File.FileID

newtype GetFileByFileID = GetFileByFileID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m GetFileByFileID File where
  dbQuery (GetFileByFileID fid) = do
    kRunAndFetch1OrThrowWhyNot fetchFile . sqlSelect "files" $ do
      mapM_ sqlResult filesSelectors
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged

newtype GetMaybeFileByFileID = GetMaybeFileByFileID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m GetMaybeFileByFileID (Maybe File) where
  dbQuery (GetMaybeFileByFileID fid) = do
    runQuery_ . sqlSelect "files" $ do
      mapM_ sqlResult filesSelectors
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged
    fetchMaybe fetchFile

-- | Insert a new 'File' in the DB without any URL nor AES configuration.
-- At the moment, it is only by 'saveNewFile' which immediately adds the missing
-- information or purges the file.
data NewEmptyFileForAWS = NewEmptyFileForAWS Text BS.ByteString
instance (MonadDB m, MonadThrow m)
  => DBUpdate m NewEmptyFileForAWS (FileID, BS.ByteString) where
  dbUpdate (NewEmptyFileForAWS fname fcontent) = do
    let fchecksum = BA.convert $ H.hashWith H.SHA1 fcontent
        fsize     = fromIntegral . BS.length $ fcontent :: Int32
    runQuery_ . sqlInsert "files" $ do
      sqlSet "name"     fname
      sqlSet "checksum" fchecksum
      sqlSet "size"     fsize
      sqlResult "id"
    fid <- fetchOne runIdentity
    return (fid, fchecksum)

data FileMovedToAWS = FileMovedToAWS FileID Text AESConf
instance (MonadDB m, MonadThrow m) => DBUpdate m FileMovedToAWS File where
  dbUpdate (FileMovedToAWS fid url aes) = do
    runQuery_ . sqlUpdate "files" $ do
      sqlSet "amazon_url" url
      sqlSet "aes_key" $ aesKey aes
      sqlSet "aes_iv" $ aesIV aes
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged
      mapM_ sqlResult filesSelectors
    fetchOne fetchFile

newtype PurgeFile = PurgeFile FileID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m PurgeFile () where
  dbUpdate (PurgeFile fid) = do
    now <- currentTime
    kRun1OrThrowWhyNot . sqlUpdate "files" $ do
      sqlSet "purged_time" now
      sqlSet "name"        ("" :: String)
      sqlSet "amazon_url"  (Nothing :: Maybe String)
      sqlWhereFileIDIs fid

filesSelectors :: [SQL]
filesSelectors = ["id", "name", "amazon_url", "checksum", "aes_key", "aes_iv", "size"]

fetchFile
  :: ( FileID
     , Text
     , Maybe Text
     , BS.ByteString
     , Maybe BS.ByteString
     , Maybe BS.ByteString
     , Int32
     )
  -> File
fetchFile (fid, fname, mamazon_url, checksum, maes_key, maes_iv, size) = File
  { fileid       = fid
  , filename     = fname
  , filestorage  =
        -- Here we need to support the following cases:
        --
        --  * encrypted data in Amazon S3: return (url, aes)
        --  * missing URL: error (see NewEmptyFileForAWS)
        --  * invalid AES key: error out at this place
    case (mamazon_url, eaes) of
      (Just url, Just (Right aes)) -> FileStorageAWS url aes
      (Just _  , Just (Left msg) ) -> err $ T.pack msg
      d ->
        unexpectedError
          $   "invalid AWS data for file with id ="
          <+> showt fid
          <>  ":"
          <+> showt d
  , filechecksum = checksum
  , filesize     = size
  }
  where
    err :: Text -> FileStorage
    err msg =
      unexpectedError
        $   "file with id ="
        <+> showt fid
        <+> "has invalid aes/iv pair:"
        <+> msg

    eaes = case (maes_key, maes_iv) of
      (Just aes_key, Just aes_iv) -> Just $ mkAESConf aes_key aes_iv
      _ -> Nothing
