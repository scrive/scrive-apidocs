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

import Crypto.Hash.RIPEMD160
import Data.ByteString.Base16
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import DB.SQL

data TableIndex = TableIndex {
  idxColumns :: [RawSQL]
, idxUnique  :: Bool
, idxWhere   :: Maybe RawSQL
} deriving (Eq, Ord, Show)

tblIndex :: TableIndex
tblIndex = TableIndex {
  idxColumns = []
, idxUnique = False
, idxWhere = Nothing
}

indexOnColumn :: RawSQL -> TableIndex
indexOnColumn column = tblIndex { idxColumns = [column] }

indexOnColumns :: [RawSQL] -> TableIndex
indexOnColumns columns = tblIndex { idxColumns = columns }

uniqueIndexOnColumn :: RawSQL -> TableIndex
uniqueIndexOnColumn column = TableIndex {
  idxColumns = [column]
, idxUnique = True
, idxWhere = Nothing
}

uniqueIndexOnColumns :: [RawSQL] -> TableIndex
uniqueIndexOnColumns columns = TableIndex {
  idxColumns = columns
, idxUnique = True
, idxWhere = Nothing
}

indexName :: RawSQL -> TableIndex -> SQL
indexName tname TableIndex{..} = mconcat [
    if idxUnique then "unique_idx__" else "idx__"
  , raw tname
  , "__"
  , intersperseNoWhitespace "__" (map raw idxColumns)
  , maybe "" (("__" <>) . hashWhere) idxWhere
  ]
  where
    -- hash WHERE clause and add it to index name so that indexes
    -- with the same columns, but different constraints can coexist
    hashWhere = raw . unsafeFromString . BS.unpack . encode . BS.take 10 . hash . BS.pack . unRawSQL

sqlCreateIndex :: RawSQL -> TableIndex -> SQL
sqlCreateIndex tname idx@TableIndex{..} = mconcat [
    "CREATE "
  , if idxUnique then "UNIQUE " else ""
  , "INDEX" <+> indexName tname idx <+> "ON" <+> raw tname <+> "("
  , intersperseNoWhitespace ", " (map raw idxColumns)
  , ")"
  , maybe "" ((" WHERE" <+>) . raw) idxWhere
  ]

sqlDropIndex :: RawSQL -> TableIndex -> SQL
sqlDropIndex tname idx = "DROP INDEX" <+> indexName tname idx
