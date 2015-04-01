module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PurgeFile(..)
    ) where

import Control.Monad.Catch
import Data.Int
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS

import Crypto
import DB
import File.Conditions
import File.File
import File.FileID
import KontraPrelude
import MinutesTime.Class

data GetFileByFileID = GetFileByFileID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m GetFileByFileID File where
  query (GetFileByFileID fid) = do
    kRunAndFetch1OrThrowWhyNot fetchFile $ sqlSelect "files" $ do
      mapM_ sqlResult filesSelectors
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged

data NewFile = NewFile String (Binary BS.ByteString)
instance (MonadDB m, MonadThrow m) => DBUpdate m NewFile FileID where
  update (NewFile filename content) = do
    runQuery_ $ sqlInsert "files" $ do
        sqlSet "name" filename
        sqlSet "content" $ content
        sqlSet "checksum" $ SHA1.hash <$> content
        sqlSet "size" (fromIntegral . BS.length $ unBinary content :: Int32)
        sqlResult "id"
    fetchOne runIdentity

data FileMovedToAWS = FileMovedToAWS FileID String String AESConf
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url aes) =
    runQuery_ $ sqlUpdate "files" $ do
        sqlSet "content" (Nothing :: Maybe (Binary BS.ByteString))
        sqlSet "amazon_bucket" bucket
        sqlSet "amazon_url" url
        sqlSet "aes_key" $ aesKey aes
        sqlSet "aes_iv" $ aesIV aes
        sqlWhereFileIDIs fid
        sqlWhereFileWasNotPurged

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance (MonadDB m, MonadThrow m) => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    runQuery_ $ sqlSelect "files" $ do
      sqlWhere "content IS NOT NULL"
      sqlLimit 1
      mapM_ sqlResult filesSelectors
    fetchMaybe fetchFile

data PurgeFile = PurgeFile FileID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m PurgeFile () where
  update (PurgeFile fid) = do
    now <- currentTime
    kRun1OrThrowWhyNot $ sqlUpdate "files" $ do
      sqlSet "purged_time" now
      sqlSetCmd "content" "NULL"
      sqlWhereFileIDIs fid

filesSelectors :: [SQL]
filesSelectors = [
    "id"
  , "name"
  , "content"
  , "amazon_bucket"
  , "amazon_url"
  , "checksum"
  , "aes_key"
  , "aes_iv"
  , "size"
  ]

fetchFile :: (FileID, String, Maybe (Binary BS.ByteString), Maybe String, Maybe String, Maybe (Binary BS.ByteString), Maybe (Binary BS.ByteString), Maybe (Binary BS.ByteString), Int32) -> File
fetchFile (fid, fname, content, amazon_bucket, amazon_url, checksum, maes_key, maes_iv, size) = File {
        fileid = fid
      , filename = fname
      , filestorage =
        -- Here we need to support the following cases:
        --
        --  * plain data in the database: just return content
        --  * encrypted data in the database (backward compatibility only):
        --      decrypt and return content
        --  * encrypted data in Amazon S3: return (bucket, url, aes)
        --  * invalid AES key: error out at this place
        --
        -- Binary data in database is temporary: next cron run should
        -- move it to Amazon.  Encrypted data in database is backward
        -- compatibility only: we are not going to put entrypted data
        -- anymore, but we need to handle some leftovers that may be
        -- lingering there.
        case content of
          Just (Binary mem) -> case eaes of
            Nothing          -> FileStorageMemory mem
            Just (Right aes) -> FileStorageMemory (aesDecrypt aes mem)
            Just (Left msg)  -> err msg
          Nothing -> case (amazon_bucket, amazon_url, eaes) of
            (Just bucket, Just url, Just (Right aes)) -> FileStorageAWS bucket url aes
            (Just _,      Just _,   Just (Left msg))  -> err msg
            d                                  -> error $ "Invalid AWS data for file with id = " ++ show fid ++ ": " ++ show d
      , filechecksum = unBinary `fmap` checksum
      , filesize = size
    }
      where
        err :: String -> FileStorage
        err msg = error $ "File with id = " ++ show fid ++ " has invalid aes/iv pair: " ++ msg
        eaes = case (maes_key, maes_iv) of
                 (Just aes_key, Just aes_iv) -> Just $ mkAESConf (unBinary aes_key) (unBinary aes_iv)
                 _ -> Nothing
