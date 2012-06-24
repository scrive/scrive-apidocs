module File.Migrations where

import Data.Int

import DB
import File.Tables
import qualified Log

addCryptoColumnsToFilesTable :: MonadDB m => Migration m
addCryptoColumnsToFilesTable = Migration {
    mgrTable = tableFiles
  , mgrFrom = 2
  , mgrDo = do
    kRunRaw "ALTER TABLE files ADD COLUMN checksum BYTEA NULL"
    kRunRaw "ALTER TABLE files ADD COLUMN aes_key BYTEA NULL"
    kRunRaw "ALTER TABLE files ADD COLUMN aes_iv BYTEA NULL"
  }

addFileIdSequence :: MonadDB m => Migration m
addFileIdSequence = Migration {
    mgrTable = tableFiles
  , mgrFrom = 1
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE files_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('files_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM files))" []
      Log.debug $ "Table files has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE files ALTER id SET DEFAULT nextval('files_id_seq')"
      return ()
  }
