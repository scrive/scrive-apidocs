module DB.Model.Index (
    TableIndex(..)
  , tblIndex
  , indexOnColumn
  , indexOnColumns
  , uniqueIndexOnColumn
  , uniqueIndexOnColumns
  , indexName
  , sqlCreateIndex
  , sqlDropIndex
  ) where

import Data.Monoid

import DB.SQL

data TableIndex = TableIndex {
  idxColumns :: [RawSQL]
, idxUnique  :: Bool
} deriving (Eq, Ord, Show)

tblIndex :: TableIndex
tblIndex = TableIndex {
  idxColumns = []
, idxUnique = False
}

indexOnColumn :: RawSQL -> TableIndex
indexOnColumn column = tblIndex { idxColumns = [column] }

indexOnColumns :: [RawSQL] -> TableIndex
indexOnColumns columns = tblIndex { idxColumns = columns }

uniqueIndexOnColumn :: RawSQL -> TableIndex
uniqueIndexOnColumn column = TableIndex {
  idxColumns = [column]
, idxUnique = True
}

uniqueIndexOnColumns :: [RawSQL] -> TableIndex
uniqueIndexOnColumns columns = TableIndex {
  idxColumns = columns
, idxUnique = True
}

indexName :: RawSQL -> TableIndex -> SQL
indexName tname TableIndex{..} = mconcat [
    if idxUnique then "unique_idx__" else "idx__"
  , raw tname
  , "__"
  , intersperseNoWhitespace "__" (map raw idxColumns)
  ]

sqlCreateIndex :: RawSQL -> TableIndex -> SQL
sqlCreateIndex tname idx@TableIndex{..} = mconcat [
    "CREATE "
  , if idxUnique then "UNIQUE " else ""
  , "INDEX" <+> indexName tname idx <+> "ON" <+> raw tname <+> "("
  , intersperseNoWhitespace ", " (map raw idxColumns)
  , ")"
  ]

sqlDropIndex :: RawSQL -> TableIndex -> SQL
sqlDropIndex tname idx = "DROP INDEX" <+> indexName tname idx
