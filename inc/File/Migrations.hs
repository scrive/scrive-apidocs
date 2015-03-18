module File.Migrations where

import DB
import File.Tables

setProperOwnerOnFilesIDSequence :: MonadDB m => Migration m
setProperOwnerOnFilesIDSequence = Migration {
    mgrTable = tableFiles
  , mgrFrom = 4
  , mgrDo = do
    runSQL_ "ALTER SEQUENCE files_id_seq OWNED BY files.id"
    runSQL_ "ALTER TABLE files ALTER aes_key DROP NOT NULL"
    runSQL_ "ALTER TABLE files ALTER aes_iv DROP NOT NULL"
}


removeDiskPathAndMakeNewColumnsNotNull :: MonadDB m => Migration m
removeDiskPathAndMakeNewColumnsNotNull = Migration {
    mgrTable = tableFiles
  , mgrFrom = 3
  , mgrDo = do
    runSQL_ "ALTER TABLE files DROP column disk_path"
    runSQL_ "ALTER TABLE files ALTER size SET NOT NULL"
    runSQL_ "ALTER TABLE files ALTER checksum SET NOT NULL"
    runSQL_ "ALTER TABLE files ALTER aes_key SET NOT NULL"
    runSQL_ "ALTER TABLE files ALTER aes_iv SET NOT NULL"
}

addCryptoColumnsToFilesTable :: MonadDB m => Migration m
addCryptoColumnsToFilesTable = Migration {
    mgrTable = tableFiles
  , mgrFrom = 2
  , mgrDo = do
    runSQL_ "ALTER TABLE files ADD COLUMN size INTEGER NULL"
    runSQL_ "ALTER TABLE files ADD COLUMN checksum BYTEA NULL"
    runSQL_ "ALTER TABLE files ADD COLUMN aes_key BYTEA NULL"
    runSQL_ "ALTER TABLE files ADD COLUMN aes_iv BYTEA NULL"
  }

addPurgedTimeToFiles :: MonadDB m => Migration m
addPurgedTimeToFiles = Migration {
    mgrTable = tableFiles
  , mgrFrom = 5
  , mgrDo = do
      -- create the sequence
      runSQL_ "ALTER TABLE files ADD COLUMN purged_time TIMESTAMPTZ NULL"
  }
