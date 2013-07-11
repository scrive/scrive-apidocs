module DB.Model where

import Data.Convertible
import Database.HDBC.SqlValue

import DB.Core
import DB.SQL (RawSQL)

data ColumnType
  = BigIntT
  | BigSerialT
  | BinaryT
  | BoolT
  | DateT
  | DoubleT
  | IntegerT
  | SmallIntT
  | TextT
  | TimestampWithZoneT
  deriving (Eq, Show)

instance Convertible SqlValue ColumnType where
  safeConvert v = do
    t <- safeConvert v
    case t of
      "bigint" -> return BigIntT
      "bytea" -> return BinaryT
      "boolean" -> return BoolT
      "date" -> return DateT
      "double precision" -> return DoubleT
      "integer" -> return IntegerT
      "smallint" -> return SmallIntT
      "text" -> return TextT
      "character varying" -> return TextT
      "timestamp with time zone" -> return TimestampWithZoneT
      _ -> Left ConvertError {
          convSourceValue = t
        , convSourceType = "String"
        , convDestType = "ColumnType"
        , convErrorMessage = "Unknown data type"
        }

data TableColumn = TableColumn {
    colName     :: RawSQL
  , colType     :: ColumnType
  , colNullable :: Bool
  , colDefault  :: Maybe RawSQL
  } deriving Show

tblColumn :: TableColumn
tblColumn = TableColumn {
    colName = error "Column name must be specified"
  , colType = error "Column type must be specified"
  , colNullable = True
  , colDefault = Nothing
  }

data TableCheck = TableCheck { chkName :: RawSQL, chkCondition :: RawSQL }
  deriving (Ord, Eq, Show)

data Table = Table {
    tblName             :: RawSQL
  , tblVersion          :: Int
  , tblColumns          :: [TableColumn]
  , tblPrimaryKey       :: [RawSQL]
  , tblUniques          :: [[RawSQL]]
  , tblChecks           :: [TableCheck]
  , tblForeignKeys      :: [ForeignKey]
  , tblIndexes          :: [TableIndex]
  , tblPutProperties    :: MonadDB m => m ()
  }

tblTable :: Table
tblTable = Table
  { tblName = error "Table name must be specified"
  , tblVersion = error "Table version must be specified"
  , tblColumns = error "Table columns must be specified"
  , tblPrimaryKey = []
  , tblUniques = []
  , tblChecks = []
  , tblForeignKeys = []
  , tblIndexes = []
  , tblPutProperties = return ()
  }

data TableIndex = TableIndex { tblIndexColumns :: [RawSQL] }
  deriving (Eq, Ord, Show)

tblTableIndex :: TableIndex
tblTableIndex = TableIndex { tblIndexColumns = [] }

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
  , mgrDo    :: m ()
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
