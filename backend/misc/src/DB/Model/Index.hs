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
import Data.Char
import Database.PostgreSQL.PQTypes
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU

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
  , mintercalate "__" $ map (asBS sanitize) idxColumns
  , maybe "" (("__" <>) . hashWhere) idxWhere
  ]
  where
    asBS f = flip rawSQL () . f . unRawSQL
    -- See http://www.postgresql.org/docs/9.4/static/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS.
    -- Remove all unallowed characters and replace them by at most one adjacent dollar sign.
    sanitize = BSU.fromString . foldr go [] . BSU.toString
      where
        go c acc = if isAlphaNum c || c == '_'
          then c : acc
          else case acc of
            ('$':_) ->       acc
            _       -> '$' : acc
    -- hash WHERE clause and add it to index name so that indexes
    -- with the same columns, but different constraints can coexist
    hashWhere = asBS $ encode . BS.take 10 . hash

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
