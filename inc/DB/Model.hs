module DB.Model where
import Database.HDBC

import DB.Core
import DB.Env
import DB.SQL (RawSQL)

data TableValidationResult = TVRvalid | TVRcreated | TVRinvalid

data Table = Table {
    tblName             :: RawSQL
  , tblVersion          :: Int
  , tblCreateOrValidate :: MonadDB m => [(String, SqlColDesc)] -> DBEnv m TableValidationResult
  , tblPutProperties    :: MonadDB m => DBEnv m ()
  , tblIndexes          :: [TableIndex]
  }

tblTable :: Table
tblTable = Table
  { tblName = error "Table name must be specified"
  , tblVersion = error "Table version must be specified"
  , tblCreateOrValidate = \_ -> return TVRinvalid
  , tblPutProperties = return ()
  , tblIndexes = []
  }

data TableIndex = TableIndex
  { tblIndexColumns     :: [String]
  }

tblTableIndex :: TableIndex
tblTableIndex = TableIndex
  { tblIndexColumns = []
  }

tblIndexOnColumn :: String -> TableIndex
tblIndexOnColumn column = TableIndex { tblIndexColumns = [column] }

-- | Migration object. Fields description:
-- * mgrTable is the table you're migrating
-- * mgrFrom is the version you're migrating from (you don't specify what
--   version you migrate TO, because version is always increased by 1, so
--   if mgrFrom is 2, that means that after that migration is run, table
--   version will equal 3
-- * mgrDo is actual body of a migration
data Migration m = Migration {
    mgrTable :: Table
  , mgrFrom  :: Int
  , mgrDo    :: DBEnv m ()
  }
