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
import Data.ByteString (ByteString)
import Data.Int
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

import Crypto
import DB
import File.Conditions
import File.FileID
import File.Types

newtype GetFileByFileID = GetFileByFileID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m GetFileByFileID File where
  dbQuery (GetFileByFileID fid) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "files" $ do
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
    fetchMaybe toComposite

-- | Insert a new 'File' in the DB without any URL nor AES configuration.
-- At the moment, it is only by 'saveNewFile' which immediately adds the missing
-- information or purges the file.
data NewEmptyFileForAWS = NewEmptyFileForAWS Text ByteString
instance (MonadDB m, MonadThrow m)
  => DBUpdate m NewEmptyFileForAWS (FileID, ByteString) where
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
    fetchOne toComposite

newtype PurgeFile = PurgeFile FileID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m PurgeFile () where
  dbUpdate (PurgeFile fid) = do
    now <- currentTime
    kRun1OrThrowWhyNot . sqlUpdate "files" $ do
      sqlSet "purged_time" now
      sqlSet "name"        ("" :: String)
      sqlSet "amazon_url"  (Nothing :: Maybe String)
      sqlWhereFileIDIs fid
