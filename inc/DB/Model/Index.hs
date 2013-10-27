module DB.Model.Index where

import Data.Monoid
import qualified Data.Set as S

import DB.SQL

data TableIndex = TableIndex {
  idxColumns :: S.Set RawSQL
, idxUnique  :: Bool
} deriving (Eq, Ord, Show)

tblIndex :: TableIndex
tblIndex = TableIndex {
  idxColumns = S.empty
, idxUnique = False
}

indexOnColumn :: RawSQL -> TableIndex
indexOnColumn column = tblIndex { idxColumns = S.singleton column }

indexOnColumns :: [RawSQL] -> TableIndex
indexOnColumns columns = tblIndex { idxColumns = S.fromList columns }

uniqueIndexOnColumn :: RawSQL -> TableIndex
uniqueIndexOnColumn column = TableIndex {
  idxColumns = S.singleton column
, idxUnique = True
}

uniqueIndexOnColumns :: [RawSQL] -> TableIndex
uniqueIndexOnColumns columns = TableIndex {
  idxColumns = S.fromList columns
, idxUnique = True
}

indexName :: RawSQL -> TableIndex -> SQL
indexName tname TableIndex{..} = mconcat [
    if idxUnique then "unique_idx__" else "idx__"
  , raw tname
  , "__"
  , intersperseNoWhitespace "__" (map raw . S.toAscList $ idxColumns)
  ]

sqlCreateIndex :: RawSQL -> TableIndex -> SQL
sqlCreateIndex tname idx@TableIndex{..} = mconcat [
    "CREATE "
  , if idxUnique then "UNIQUE " else ""
  , "INDEX" <+> indexName tname idx <+> "ON" <+> raw tname <+> "("
  , intersperseNoWhitespace ", " (map raw . S.toAscList $ idxColumns)
  , ")"
  ]

sqlDropIndex :: RawSQL -> TableIndex -> SQL
sqlDropIndex tname idx = "DROP INDEX" <+> indexName tname idx
