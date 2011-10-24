{-# OPTIONS_GHC -fno-warn-orphans #-}
module File.Model
    ( FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PutFileUnchecked(..)
    ) where

import Control.Applicative
import Database.HDBC
import qualified Data.ByteString.Char8 as BS

import DB.Classes
import DB.Utils
import DB.Types
import File.File
import File.FileID
import File.Tables
import Data.Maybe

fetchFiles :: Statement -> [File] -> IO [File]
fetchFiles st acc = fetchRow st >>= maybe (return acc) f
    where f [ fid, fname, content, amazon_bucket, amazon_url, disk_path
            ] = fetchFiles st $ File { fileid = fromSql fid
                                     , filename = fromSql fname
                                     , filestorage = case fromSql content of
                                                         Just mem -> FileStorageMemory (unBinary mem)
                                                         Nothing -> case fromSql disk_path of
                                                                        Just path -> FileStorageDisk path
                                                                        Nothing -> case (fromSql amazon_bucket, fromSql amazon_url) of
                                                                                       (Just bucket, Just url) -> FileStorageAWS bucket url
                                                                                       _ -> FileStorageMemory BS.empty
                                     } : acc
          f x = error $ "fetchFiles got a row with " ++ show (length x) ++ " did expect 6"

data GetFileByFileID = GetFileByFileID FileID
instance DBQuery GetFileByFileID (Maybe File) where
  dbQuery (GetFileByFileID fid) = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT id, name, encode(content,'base64'), amazon_bucket, amazon_url, disk_path FROM files WHERE id = ?"
    _ <- execute st [toSql fid]
    files <- fetchFiles st []
    oneObjectReturnedGuard files

data NewFile = NewFile BS.ByteString BS.ByteString
instance DBUpdate NewFile (File) where
  dbUpdate (NewFile filename content) = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE files IN ACCESS EXCLUSIVE MODE"
    fid <- FileID <$> getUniqueID tableFiles
    wrapDB $ \conn -> do
          _ <- run conn ("INSERT INTO files ("
            ++ "  id"
            ++ ", name"
            ++ ", content"
            ++ ") VALUES (?, ?, decode(?,'base64'))") $ [
                toSql fid
              , toSql $ filename
              , toSql $ Binary content
              ]
          return ()
    x <- dbQuery $ GetFileByFileID fid
    case x of
        Nothing -> error "impossible"
        Just file -> return file

data PutFileUnchecked = PutFileUnchecked File
instance DBUpdate PutFileUnchecked () where
  dbUpdate (PutFileUnchecked file) = do
    wrapDB $ \conn -> do
          _ <- run conn ("INSERT INTO files (id, name, content, amazon_bucket, amazon_url, disk_path) VALUES (?,?,decode(?,'base64'),?,?,?)") $
               [toSql $ fileid file, toSql $ filename file] ++
               case filestorage file of
                   FileStorageMemory mem -> [toSql (Binary mem),SqlNull,SqlNull,SqlNull]
                   FileStorageAWS bucket url -> [SqlNull,toSql bucket,toSql url,SqlNull]
                   FileStorageDisk path -> [SqlNull,SqlNull,SqlNull,toSql path]
          return ()
    return ()

data FileMovedToAWS = FileMovedToAWS FileID BS.ByteString BS.ByteString
instance DBUpdate FileMovedToAWS () where
  dbUpdate (FileMovedToAWS fid bucket url) = wrapDB $ \conn -> do
        st <- prepare conn "UPDATE files SET content = NULL, amazon_bucket = ?, amazon_url = ? WHERE id = ?"
        r <- execute st [toSql bucket, toSql url, toSql fid]
        _ <- oneRowAffectedGuard r
        return ()

data FileMovedToDisk = FileMovedToDisk FileID FilePath
instance DBUpdate FileMovedToDisk () where
  dbUpdate (FileMovedToDisk fid path) = wrapDB $ \conn -> do
        st <- prepare conn "UPDATE files SET content = NULL, disk_path = ? WHERE id = ?"
        r <- execute st [toSql path, toSql fid]
        _ <- oneRowAffectedGuard r
        return ()

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance DBQuery GetFileThatShouldBeMovedToAmazon (Maybe File) where
  dbQuery GetFileThatShouldBeMovedToAmazon = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT id, name, content, amazon_bucket, amazon_url, disk_path " ++
                         "FROM files " ++
                         "WHERE content IS NOT NULL " ++
                         "LIMIT 1"
    _ <- execute st []
    files <- fetchFiles st []
    return $ listToMaybe files
