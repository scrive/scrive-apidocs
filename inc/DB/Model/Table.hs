module DB.Model.Table where

import Data.Convertible
import Data.Char
import Database.HDBC.SqlValue

import DB.Core
import DB.Model.Check
import DB.Model.Index
import DB.Model.ForeignKey
import DB.SQL

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
    case map toLower t of
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

data Table = Table {
  tblName          :: RawSQL
, tblVersion       :: Int
, tblColumns       :: [TableColumn]
, tblPrimaryKey    :: [RawSQL]
, tblChecks        :: [TableCheck]
, tblForeignKeys   :: [ForeignKey]
, tblIndexes       :: [TableIndex]
, tblPutProperties :: MonadDB m => m ()
}

tblColumn :: TableColumn
tblColumn = TableColumn {
  colName = error "Column name must be specified"
, colType = error "Column type must be specified"
, colNullable = True
, colDefault = Nothing
}

tblTable :: Table
tblTable = Table {
  tblName = error "Table name must be specified"
, tblVersion = error "Table version must be specified"
, tblColumns = error "Table columns must be specified"
, tblPrimaryKey = []
, tblChecks = []
, tblForeignKeys = []
, tblIndexes = []
, tblPutProperties = return ()
}
