module DB.Model where
import Database.HDBC

import DB.Core
import DB.Env
import DB.SQL (RawSQL)

data TableValidationResult = TVRvalid | TVRcreated | TVRinvalid
  deriving (Eq, Ord, Show)

data Table = Table {
    tblName             :: RawSQL
  , tblVersion          :: Int
  , tblCreateOrValidate :: MonadDB m => [(String, SqlColDesc)] -> DBEnv m TableValidationResult
  , tblPutProperties    :: MonadDB m => DBEnv m ()
  , tblIndexes          :: [TableIndex]
  , tblForeignKeys      :: [ForeignKey]
  }

tblTable :: Table
tblTable = Table
  { tblName = error "Table name must be specified"
  , tblVersion = error "Table version must be specified"
  , tblCreateOrValidate = \_ -> return TVRinvalid
  , tblPutProperties = return ()
  , tblIndexes = []
  , tblForeignKeys = []
  }

data TableIndex = TableIndex
  { tblIndexColumns     :: [RawSQL]
  }
  deriving (Eq, Ord, Show)

tblTableIndex :: TableIndex
tblTableIndex = TableIndex
  { tblIndexColumns = []
  }

tblIndexOnColumn :: RawSQL -> TableIndex
tblIndexOnColumn column = TableIndex { tblIndexColumns = [column] }

tblIndexOnColumns :: [RawSQL] -> TableIndex
tblIndexOnColumns columns = TableIndex { tblIndexColumns = columns }

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

data ForeignKey = ForeignKey
  { fkColumns    :: [RawSQL]
  , fkRefTable   :: RawSQL
  , fkRefColumns :: [RawSQL]
  , fkOnUpdate   :: ForeignKeyAction
  , fkOnDelete   :: ForeignKeyAction
  , fkDeferrable :: Bool
  , fkDeferred   :: Bool
  }
  deriving (Eq, Ord, Show)

data ForeignKeyAction
  = ForeignKeyNoAction
  | ForeignKeyRestrict
  | ForeignKeyCascade
  | ForeignKeySetNull
  | ForeignKeySetDefault
  deriving (Eq, Ord, Show, Read)


tblForeignKeyColumn :: RawSQL -> RawSQL -> RawSQL -> ForeignKey
tblForeignKeyColumn column reftable refcolumn =
  tblForeignKeyColumns [column] reftable [refcolumn]

tblForeignKeyColumns :: [RawSQL] -> RawSQL -> [RawSQL] -> ForeignKey
tblForeignKeyColumns columns reftable refcolumns =
  ForeignKey
  { fkColumns    = columns
  , fkRefTable   = reftable
  , fkRefColumns = refcolumns
  , fkOnUpdate   = ForeignKeyCascade
  , fkOnDelete   = ForeignKeyNoAction
  , fkDeferrable = True
  , fkDeferred   = False
  }
