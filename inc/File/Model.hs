module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    ) where

import Control.Applicative
import Data.List
import Database.HDBC
import qualified Data.ByteString.Char8 as BS

import Crypto
import DB
import File.File
import File.FileID
import File.Tables
import Misc
import qualified Crypto.Hash.SHA1 as SHA1

data GetFileByFileID = GetFileByFileID FileID
instance MonadDB m => DBQuery m GetFileByFileID (Maybe File) where
  query (GetFileByFileID fid) = do
    kRun_ $ selectFilesSQL <++> SQL "WHERE id = ?" [toSql fid]
    fetchFiles >>= oneObjectReturnedGuard

data NewFile = NewFile String Binary
instance MonadDB m => DBUpdate m NewFile File where
  update (NewFile filename content) = do
     kPrepare $ "INSERT INTO files"
       ++ "( name"
       ++ ", content"
       ++ ", checksum"
       ++ ") SELECT ?, decode(?,'base64'), decode(?,'base64')"
       ++ " RETURNING " ++ filesSelectors
     _ <- kExecute
      [ toSql filename
      , toSql content
      , toSql . Binary . SHA1.hash $ unBinary content
      ]
     fetchFiles >>= exactlyOneObjectReturnedGuard

data FileMovedToAWS = FileMovedToAWS FileID String String AESConf
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url aes) =
    kRun_ $ mkSQL UPDATE tableFiles [
        sql "content" SqlNull
      , sql "amazon_bucket" bucket
      , sql "amazon_url" url
      , sql "aes_key" $ aesKey aes
      , sql "aes_iv" $ aesIV aes
      ] <++> SQL "WHERE id = ?" [toSql fid]

data FileMovedToDisk = FileMovedToDisk FileID FilePath
instance MonadDB m => DBUpdate m FileMovedToDisk () where
  update (FileMovedToDisk fid path) = do
    kPrepare "UPDATE files SET content = NULL, disk_path = ? WHERE id = ?"
    _ <- kExecute1 [toSql path, toSql fid]
    return ()

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance MonadDB m => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    kRun_ $ selectFilesSQL <++> SQL "WHERE content IS NOT NULL LIMIT 1" []
    fetchFiles >>= oneObjectReturnedGuard

selectFilesSQL :: SQL
selectFilesSQL = SQL ("SELECT " ++ filesSelectors ++ " FROM files ") []

filesSelectors :: String
filesSelectors = intercalate ", " [
    "id"
  , "name"
  , "encode(content, 'base64')"
  , "amazon_bucket"
  , "amazon_url"
  , "disk_path"
  , "encode(checksum, 'base64')"
  , "encode(aes_key, 'base64')"
  , "encode(aes_iv, 'base64')"
  ]

fetchFiles :: MonadDB m => DBEnv m [File]
fetchFiles = foldDB decoder []
  where
    decoder acc fid fname content amazon_bucket amazon_url disk_path checksum aes_key aes_iv = File {
        fileid = fid
      , filename = fname
      , filestorage =
        case content of
          Just mem -> FileStorageMemory (unBinary mem)
          Nothing -> case disk_path of
            Just path -> FileStorageDisk path
            Nothing -> case (amazon_bucket, amazon_url, mkAESConf <$> unBinary `fmap` aes_key <*> unBinary `fmap` aes_iv) of
              (Just bucket, Just url, Just (Right aes)) -> FileStorageAWS bucket url aes
              _ -> FileStorageMemory BS.empty
      , filechecksum = checksum
    } : acc
