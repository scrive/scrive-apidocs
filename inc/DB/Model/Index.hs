module DB.Model.Index where

import qualified Data.Set as S
import DB.SQL

data TableIndex = TableIndex {
  tblIndexColumns :: S.Set RawSQL
, tblIndexUnique  :: Bool
} deriving (Eq, Ord, Show)

tblTableIndex :: TableIndex
tblTableIndex = TableIndex { tblIndexColumns = S.empty, tblIndexUnique = False }

tblIndexOnColumn :: RawSQL -> TableIndex
tblIndexOnColumn column = tblTableIndex { tblIndexColumns = S.singleton column }

tblIndexOnColumns :: [RawSQL] -> TableIndex
tblIndexOnColumns columns = tblTableIndex { tblIndexColumns = S.fromList columns }

tblUniqueIndexOnColumn :: RawSQL -> TableIndex
tblUniqueIndexOnColumn column = TableIndex {
  tblIndexColumns = S.singleton column
, tblIndexUnique = True
}

tblUniqueIndexOnColumns :: [RawSQL] -> TableIndex
tblUniqueIndexOnColumns columns = TableIndex {
  tblIndexColumns = S.fromList columns
, tblIndexUnique = True
}
