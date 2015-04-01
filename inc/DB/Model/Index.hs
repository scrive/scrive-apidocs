module DB.Model.Index (
    TableIndex(..)
  , tblIndex
  , indexOnColumn
  , indexOnColumns
  , uniqueIndexOnColumn
  , uniqueIndexOnColumnWithCondition
  , uniqueIndexOnColumns
  , indexName
  , sqlCreateIndex
  , sqlDropIndex
  ) where

import Crypto.Hash.RIPEMD160
import Data.ByteString.Base16
import Database.PostgreSQL.PQTypes
import qualified Data.ByteString.Char8 as BS

import KontraPrelude

data TableIndex = TableIndex {
  idxColumns :: [RawSQL ()]
, idxUnique  :: Bool
, idxWhere   :: Maybe (RawSQL ())
} deriving (Eq, Ord, Show)

tblIndex :: TableIndex
tblIndex = TableIndex {
  idxColumns = []
, idxUnique = False
, idxWhere = Nothing
}

indexOnColumn :: RawSQL () -> TableIndex
indexOnColumn column = tblIndex { idxColumns = [column] }

indexOnColumns :: [RawSQL ()] -> TableIndex
indexOnColumns columns = tblIndex { idxColumns = columns }

uniqueIndexOnColumn :: RawSQL () -> TableIndex
uniqueIndexOnColumn column = TableIndex {
  idxColumns = [column]
, idxUnique = True
, idxWhere = Nothing
}

uniqueIndexOnColumns :: [RawSQL ()] -> TableIndex
uniqueIndexOnColumns columns = TableIndex {
  idxColumns = columns
, idxUnique = True
, idxWhere = Nothing
}

uniqueIndexOnColumnWithCondition :: RawSQL () -> RawSQL () -> TableIndex
uniqueIndexOnColumnWithCondition column whereC = TableIndex {
  idxColumns = [column]
, idxUnique = True
, idxWhere = Just whereC
}

indexName :: RawSQL () -> TableIndex -> RawSQL ()
indexName tname TableIndex{..} = flip rawSQL () $ BS.take 63 . unRawSQL $ mconcat [
    if idxUnique then "unique_idx__" else "idx__"
  , tname
  , "__"
  , mintercalate "__" idxColumns
  , maybe "" (("__" <>) . hashWhere) idxWhere
  ]
  where
    -- hash WHERE clause and add it to index name so that indexes
    -- with the same columns, but different constraints can coexist
    hashWhere = flip rawSQL () . encode . BS.take 10 . hash . unRawSQL

sqlCreateIndex :: RawSQL () -> TableIndex -> RawSQL ()
sqlCreateIndex tname idx@TableIndex{..} = mconcat [
    "CREATE "
  , if idxUnique then "UNIQUE " else ""
  , "INDEX" <+> indexName tname idx <+> "ON" <+> tname <+> "("
  , mintercalate ", " idxColumns
  , ")"
  , maybe "" (" WHERE" <+>) idxWhere
  ]

sqlDropIndex :: RawSQL () -> TableIndex -> RawSQL ()
sqlDropIndex tname idx = "DROP INDEX" <+> indexName tname idx
