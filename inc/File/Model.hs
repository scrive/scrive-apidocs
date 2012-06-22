module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    ) where

import Database.HDBC
import qualified Data.ByteString.Char8 as BS

import DB
import File.File
import File.FileID

data GetFileByFileID = GetFileByFileID FileID
instance MonadDB m => DBQuery m GetFileByFileID (Maybe File) where
  query (GetFileByFileID fid) = do
    kPrepare $ "SELECT id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path FROM files WHERE id = ?"
    _ <- kExecute [toSql fid]
    fetchFiles >>= oneObjectReturnedGuard

data NewFile = NewFile String Binary
instance MonadDB m => DBUpdate m NewFile File where
  update (NewFile filename content) = do
     kPrepare $ "INSERT INTO files"
       ++ "( name"
       ++ ", content"
       ++ ") SELECT ?, decode(?,'base64')"
       ++ " RETURNING id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path"
     _ <- kExecute
      [ toSql filename
      , toSql content
      ]
     fs <- fetchFiles
     case fs of
            [file] -> return file
            _ ->  update (NewFile filename content)

data FileMovedToAWS = FileMovedToAWS FileID String String
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url) = do
    kPrepare "UPDATE files SET content = NULL, amazon_bucket = ?, amazon_url = ? WHERE id = ?"
    _ <- kExecute1 [toSql bucket, toSql url, toSql fid]
    return ()

data FileMovedToDisk = FileMovedToDisk FileID FilePath
instance MonadDB m => DBUpdate m FileMovedToDisk () where
  update (FileMovedToDisk fid path) = do
    kPrepare "UPDATE files SET content = NULL, disk_path = ? WHERE id = ?"
    _ <- kExecute1 [toSql path, toSql fid]
    return ()

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance MonadDB m => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    kPrepare $ "SELECT id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path FROM files WHERE content IS NOT NULL LIMIT 1"
    _ <- kExecute []
    fetchFiles >>= oneObjectReturnedGuard

fetchFiles :: MonadDB m => DBEnv m [File]
fetchFiles = foldDB decoder []
  where
    decoder acc fid fname content amazon_bucket amazon_url disk_path = File {
        fileid = fid
      , filename = fname
      , filestorage =
        case content of
          Just mem -> FileStorageMemory (unBinary mem)
          Nothing -> case disk_path of
            Just path -> FileStorageDisk path
            Nothing -> case (amazon_bucket, amazon_url) of
              (Just bucket, Just url) -> FileStorageAWS bucket url
              _ -> FileStorageMemory BS.empty
    } : acc
