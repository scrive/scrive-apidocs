module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PurgeFile(..)
    ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS

import Crypto
import DB
import File.Conditions
import File.File
import File.FileID
import KontraPrelude

data GetFileByFileID = GetFileByFileID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m GetFileByFileID File where
  query (GetFileByFileID fid) = do
    kRunAndFetch1OrThrowWhyNot fetchFile $ sqlSelect "files" $ do
      mapM_ sqlResult filesSelectors
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged

data NewFile = NewFile String BS.ByteString
instance (MonadDB m, MonadThrow m) => DBUpdate m NewFile FileID where
  update (NewFile filename content) = do
    runQuery_ $ sqlInsert "files" $ do
        sqlSet "name" filename
        sqlSet "content" $ content
        sqlSet "checksum" $ SHA1.hash content
        sqlSet "size" (fromIntegral . BS.length $ content :: Int32)
        sqlResult "id"
    fetchOne runIdentity

data FileMovedToAWS = FileMovedToAWS FileID String AESConf
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid url aes) =
    runQuery_ $ sqlUpdate "files" $ do
        sqlSet "content" (Nothing :: Maybe BS.ByteString)
        sqlSet "amazon_url" url
        sqlSet "aes_key" $ aesKey aes
        sqlSet "aes_iv" $ aesIV aes
        sqlWhereFileIDIs fid
        sqlWhereFileWasNotPurged

data GetFilesThatShouldBeMovedToAmazon = GetFilesThatShouldBeMovedToAmazon Int
instance (MonadDB m, MonadThrow m) => DBQuery m GetFilesThatShouldBeMovedToAmazon [File] where
  query (GetFilesThatShouldBeMovedToAmazon n) = do
    runQuery_ $ sqlSelect "files" $ do
      mapM_ sqlResult filesSelectors
      sqlWhere "content IS NOT NULL"
      sqlOrderBy "id"
      sqlLimit n
    fetchMany fetchFile

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
  , "amazon_url"
  , "checksum"
  , "aes_key"
  , "aes_iv"
  , "size"
  ]

fetchFile :: (FileID, String, Maybe BS.ByteString, Maybe String, Maybe BS.ByteString, Maybe BS.ByteString, Maybe BS.ByteString, Int32) -> File
fetchFile (fid, fname, content, amazon_url, checksum, maes_key, maes_iv, size) = File {
        fileid = fid
      , filename = fname
      , filestorage =
        -- Here we need to support the following cases:
        --
        --  * plain data in the database: just return content
        --  * encrypted data in the database (backward compatibility only):
        --      decrypt and return content
        --  * encrypted data in Amazon S3: return (url, aes)
        --  * invalid AES key: error out at this place
        --
        -- Binary data in database is temporary: next cron run should
        -- move it to Amazon.  Encrypted data in database is backward
        -- compatibility only: we are not going to put entrypted data
        -- anymore, but we need to handle some leftovers that may be
        -- lingering there.
        case content of
          Just mem -> case eaes of
            Nothing          -> FileStorageMemory mem
            Just (Right aes) -> FileStorageMemory (aesDecrypt aes mem)
            Just (Left msg)  -> err msg
          Nothing -> case (amazon_url, eaes) of
            (Just url, Just (Right aes)) -> FileStorageAWS url aes
            (Just _,   Just (Left msg))  -> err msg
            d                                  -> $unexpectedError $ "invalid AWS data for file with id =" <+> show fid <> ":" <+> show d
      , filechecksum = checksum
      , filesize = size
    }
      where
        err :: String -> FileStorage
        err msg = $unexpectedError $ "file with id =" <+> show fid <+> "has invalid aes/iv pair:" <+> msg
        eaes = case (maes_key, maes_iv) of
                 (Just aes_key, Just aes_iv) -> Just $ mkAESConf aes_key aes_iv
                 _ -> Nothing
