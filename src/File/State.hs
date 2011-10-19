{-# OPTIONS_GHC -fno-warn-orphans #-}
module File.State 
    ( FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PutFileUnchecked(..)
    , GetAllFilesForDBMigration(..)
    , Files
    , populateDBWithFilesIfEmpty
    ) where

import File.FileID
import Data.Data
import qualified Data.ByteString.UTF8 as BS
import Happstack.Data.IxSet as IxSet
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Misc
import File.File
import Control.Monad
import Database.HDBC
import DB.Classes
import DB.Types
import qualified AppLogger as Log

type Files = IxSet File

data FileMovePending = FileMovePending
                     deriving (Eq, Ord, Typeable)

instance Indexable File where
  empty = 
    ixSet [ ixFun (\x -> [fileid x] :: [FileID])
          , ixFun (\x -> case x of
                             File{ filestorage = FileStorageMemory{} } -> [FileMovePending]
                             _ -> [])
          ]

instance Component Files where
  type Dependencies Files = End
  initialValue = empty

getFileByFileID :: FileID -> Query Files (Maybe File)
getFileByFileID fid = ask >>= \files ->
    return $ getOne $ files @= fid

newFile :: BS.ByteString -> BS.ByteString -> Update Files File
newFile name content = do
  files <- ask
  fid <- getUnique64 files FileID
  let file = File { fileid = fid
                  , filename = name
                  , filestorage = FileStorageMemory content
                  }
  modify $ insert file
  return file

putFileUnchecked :: File -> Update Files FileID
putFileUnchecked file = do
  modify $ insert file
  return (fileid file)

fileMovedToAWS :: FileID
               -> BS.ByteString
               -> BS.ByteString
               -> Update Files ()
fileMovedToAWS fileid bucket url = fileMovedTo fileid $ FileStorageAWS bucket url

fileMovedToDisk :: FileID -> FilePath -> Update Files ()
fileMovedToDisk fileid filepath = fileMovedTo fileid $ FileStorageDisk filepath

fileMovedTo :: FileID -> FileStorage -> Update Files ()
fileMovedTo fid fstorage = do
    files <- ask
    case getOne (files @= fid) of
        Nothing -> return ()
        Just file -> do
            modify (updateIx fid $ file { filestorage = fstorage } )
            return ()

getFilesThatShouldBeMovedToAmazon :: Query Files [File]
getFilesThatShouldBeMovedToAmazon = do
    files <- ask
    return (IxSet.toList (files @= FileMovePending))

getAllFilesForDBMigration :: Query Files [File]
getAllFilesForDBMigration = do
    files <- ask
    return (IxSet.toList files)

$(mkMethods ''Files [ 'getFileByFileID
                    , 'newFile
                    , 'fileMovedToAWS
                    , 'fileMovedToDisk
                    , 'getFilesThatShouldBeMovedToAmazon
                    , 'putFileUnchecked
                    , 'getAllFilesForDBMigration
                    ])

initialInsertFilesIntoPG :: DB ()
initialInsertFilesIntoPG = wrapDB $ \conn -> do
  files <- query GetAllFilesForDBMigration
  forM_ files $ \file -> do
    Log.debug $ show file
    _ <- run conn ("INSERT INTO files (id, name, content, amazon_bucket, amazon_url, disk_path) VALUES (?,?,decode(?,'base64'),?,?,?)") $
               [toSql $ fileid file, toSql $ filename file] ++
               case filestorage file of
                   FileStorageMemory mem -> [toSql (Binary mem), SqlNull, SqlNull, SqlNull]
                   FileStorageAWS bucket url -> [SqlNull, toSql bucket, toSql url, SqlNull]
                   FileStorageDisk path -> [SqlNull, SqlNull, SqlNull, toSql path]
    return ()

populateDBWithFilesIfEmpty :: DB ()
populateDBWithFilesIfEmpty = do
  n <- wrapDB $ \conn -> do
    st <- prepare conn "SELECT COUNT(*) FROM files"
    _ <- executeRaw st
    [n] <- fetchAllRows' st >>= return . map fromSql . join
    return (n::Int)
  when (n == 0) $ do
    Log.debug "No files in database, populating with values from happstack-state..."
    Log.debug "Copying files..."
    initialInsertFilesIntoPG
    Log.debug "Done."
