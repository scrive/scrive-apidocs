module DB.Model.Index where

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
