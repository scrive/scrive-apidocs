{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PutFileUnchecked(..)
    ) where

import Database.HDBC
import qualified Data.ByteString.Char8 as BS

import DB.Classes
import DB.Fetcher2
import DB.Types
import DB.Utils
import File.File
import File.FileID
import OurPrelude

data GetFileByFileID = GetFileByFileID FileID
instance DBQuery GetFileByFileID (Maybe File) where
  dbQuery (GetFileByFileID fid) = do
    kPrepare $ "SELECT id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path FROM files WHERE id = ?"
    _ <- kExecute [toSql fid]
    fetchFiles >>= oneObjectReturnedGuard

data NewFile = NewFile String BS.ByteString
instance DBUpdate NewFile File where
  dbUpdate (NewFile filename content) = do
     kPrepare $ "INSERT INTO files"
       ++ "( name"
       ++ ", content"
       ++ ") SELECT ?, decode(?,'base64')"
       ++ " RETURNING id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path"
     _ <- kExecute
      [ toSql filename
      , toSql (Binary content)
      ]
     fs <- fetchFiles
     case fs of
            [file] -> return file
            _ ->  dbUpdate (NewFile filename content) 

data PutFileUnchecked = PutFileUnchecked File
instance DBUpdate PutFileUnchecked () where
  dbUpdate (PutFileUnchecked file) = do
    kPrepare ("INSERT INTO files (id, name, content, amazon_bucket, amazon_url, disk_path) VALUES (?,?,decode(?,'base64'),?,?,?)") 
    _ <- kExecute1 $ [ toSql (fileid file)
                     , toSql (filename file)
                     ] ++ case filestorage file of
                            FileStorageMemory mem ->
                              [toSql (Binary mem),SqlNull,SqlNull,SqlNull]
                            FileStorageAWS bucket url ->
                              [SqlNull,toSql bucket,toSql url,SqlNull]
                            FileStorageDisk path ->
                              [SqlNull,SqlNull,SqlNull,toSql path]
    return ()

data FileMovedToAWS = FileMovedToAWS FileID String String
instance DBUpdate FileMovedToAWS () where
  dbUpdate (FileMovedToAWS fid bucket url) = do 
    kPrepare "UPDATE files SET content = NULL, amazon_bucket = ?, amazon_url = ? WHERE id = ?"
    _ <- kExecute1 [toSql bucket, toSql url, toSql fid]
    return ()

data FileMovedToDisk = FileMovedToDisk FileID FilePath
instance DBUpdate FileMovedToDisk () where
  dbUpdate (FileMovedToDisk fid path) = do
    kPrepare "UPDATE files SET content = NULL, disk_path = ? WHERE id = ?"
    _ <- kExecute1 [toSql path, toSql fid]
    return ()

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance DBQuery GetFileThatShouldBeMovedToAmazon (Maybe File) where
  dbQuery GetFileThatShouldBeMovedToAmazon = do
    kPrepare $ "SELECT id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path FROM files WHERE content IS NOT NULL LIMIT 1"
    _ <- kExecute []
    fetchFiles >>= oneObjectReturnedGuard

fetchFiles :: DB [File]
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
