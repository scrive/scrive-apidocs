module DB.Model.ColumnType (
    ColumnType(..)
  , columnTypeToSQL
  ) where

import Data.Char
import Database.PostgreSQL.PQTypes

import KontraPrelude

data ColumnType
  = BigIntT
  | BigSerialT
  | BinaryT
  | BoolT
  | DateT
  | DoubleT
  | IntegerT
  | IntervalT
  | JsonT
  | JsonbT
  | SmallIntT
  | TextT
  | TimestampWithZoneT
  | XmlT
  | ArrayT !ColumnType
  | CustomT !(RawSQL ())
    deriving (Eq, Ord, Show)

instance PQFormat ColumnType where
  pqFormat = const $ pqFormat ($undefined::String)
instance FromSQL ColumnType where
  type PQBase ColumnType = PQBase String
  fromSQL mbase = parseType . map toLower <$> fromSQL mbase
    where
      parseType = \case
        "bigint" -> BigIntT
        "bytea" -> BinaryT
        "boolean" -> BoolT
        "date" -> DateT
        "double precision" -> DoubleT
        "integer" -> IntegerT
        "interval" -> IntervalT
        "json" -> JsonT
        "jsonb" -> JsonbT
        "smallint" -> SmallIntT
        "text" -> TextT
        "timestamp with time zone" -> TimestampWithZoneT
        "xml" -> XmlT
        tname
          | "[]" `isSuffixOf` tname -> ArrayT . parseType . init . init $ tname
          | otherwise -> CustomT $ unsafeSQL tname

columnTypeToSQL :: ColumnType -> RawSQL ()
columnTypeToSQL BigIntT = "BIGINT"
columnTypeToSQL BigSerialT = "BIGSERIAL"
columnTypeToSQL BinaryT = "BYTEA"
columnTypeToSQL BoolT = "BOOLEAN"
columnTypeToSQL DateT = "DATE"
columnTypeToSQL DoubleT = "DOUBLE PRECISION"
columnTypeToSQL IntegerT = "INTEGER"
columnTypeToSQL IntervalT = "INTERVAL"
columnTypeToSQL JsonT = "JSON"
columnTypeToSQL JsonbT = "JSONB"
columnTypeToSQL SmallIntT = "SMALLINT"
columnTypeToSQL TextT = "TEXT"
columnTypeToSQL TimestampWithZoneT = "TIMESTAMPTZ"
columnTypeToSQL XmlT = "XML"
columnTypeToSQL (ArrayT t) = columnTypeToSQL t <> "[]"
columnTypeToSQL (CustomT tname) = tname
