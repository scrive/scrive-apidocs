{-# LANGUAGE ExistentialQuantification #-}
module DB.Model.Table (
    ColumnType(..)
  , columnTypeToSQL
  , TableColumn(..)
  , tblColumn
  , sqlAddColumn
  , sqlAlterColumn
  , sqlDropColumn
  , Rows(..)
  , Table(..)
  , tblTable
  , sqlCreateTable
  , sqlAlterTable
  ) where

import Data.ByteString (ByteString)
import Data.Char
import Data.Int
import Data.Monoid.Space
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

import DB.Model.Check
import DB.Model.ForeignKey
import DB.Model.Index
import DB.Model.PrimaryKey

data ColumnType
  = BigIntT
  | BigSerialT
  | BinaryT
  | BoolT
  | DateT
  | DoubleT
  | IntegerT
  | IntervalT
  | SmallIntT
  | TextT
  | VarCharT -- for compatibility, do not use this
  | TimestampWithZoneT
  deriving (Eq, Show)

instance PQFormat ColumnType where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL ColumnType where
  type PQBase ColumnType = PQBase String
  fromSQL mbase = do
    t <- fromSQL mbase
    case map toLower t of
      "bigint" -> return BigIntT
      "bytea" -> return BinaryT
      "boolean" -> return BoolT
      "date" -> return DateT
      "double precision" -> return DoubleT
      "integer" -> return IntegerT
      "interval" -> return IntervalT
      "smallint" -> return SmallIntT
      "text" -> return TextT
      "character varying" -> return VarCharT
      "timestamp with time zone" -> return TimestampWithZoneT
      _ -> hpqTypesError $ "Unknown data type: " ++ t

columnTypeToSQL :: ColumnType -> RawSQL ()
columnTypeToSQL BigIntT = "BIGINT"
columnTypeToSQL BigSerialT = "BIGSERIAL"
columnTypeToSQL BinaryT = "BYTEA"
columnTypeToSQL BoolT = "BOOLEAN"
columnTypeToSQL DateT = "DATE"
columnTypeToSQL DoubleT = "DOUBLE PRECISION"
columnTypeToSQL IntegerT = "INTEGER"
columnTypeToSQL IntervalT = "INTERVAL"
columnTypeToSQL SmallIntT = "SMALLINT"
columnTypeToSQL TextT = "TEXT"
columnTypeToSQL VarCharT = "VARCHAR"
columnTypeToSQL TimestampWithZoneT = "TIMESTAMPTZ"

----------------------------------------

data TableColumn = TableColumn {
  colName     :: RawSQL ()
, colType     :: ColumnType
, colNullable :: Bool
, colDefault  :: Maybe (RawSQL ())
} deriving Show

tblColumn :: TableColumn
tblColumn = TableColumn {
  colName = error "Column name must be specified"
, colType = error "Column type must be specified"
, colNullable = True
, colDefault = Nothing
}

sqlAddColumn :: TableColumn -> RawSQL ()
sqlAddColumn TableColumn{..} = smconcat [
    "ADD COLUMN"
  , colName
  , columnTypeToSQL colType
  , if colNullable then "NULL" else "NOT NULL"
  , maybe "" ("DEFAULT" <+>) colDefault
  ]

sqlAlterColumn :: RawSQL () -> RawSQL () -> RawSQL ()
sqlAlterColumn cname alter = "ALTER COLUMN" <+> cname <+> alter

sqlDropColumn :: RawSQL () -> RawSQL ()
sqlDropColumn cname = "DROP COLUMN" <+> cname

----------------------------------------

data Rows = forall row. (Show row, ToRow row) => Rows [ByteString] [row]

data Table = Table {
  tblName          :: RawSQL ()
, tblVersion       :: Int32
, tblColumns       :: [TableColumn]
, tblPrimaryKey    :: Maybe PrimaryKey
, tblChecks        :: [TableCheck]
, tblForeignKeys   :: [ForeignKey]
, tblIndexes       :: [TableIndex]
, tblInitialData   :: Maybe Rows
}

tblTable :: Table
tblTable = Table {
  tblName = error "Table name must be specified"
, tblVersion = error "Table version must be specified"
, tblColumns = error "Table columns must be specified"
, tblPrimaryKey = Nothing
, tblChecks = []
, tblForeignKeys = []
, tblIndexes = []
, tblInitialData = Nothing
}

sqlCreateTable :: RawSQL () -> RawSQL ()
sqlCreateTable tname = "CREATE TABLE" <+> tname <+> "()"

sqlAlterTable :: RawSQL () -> [RawSQL ()] -> RawSQL ()
sqlAlterTable tname alter_statements = smconcat [
    "ALTER TABLE"
  , tname
  , mintercalate ", " alter_statements
  ]
