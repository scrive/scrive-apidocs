module DB.Model.PrimaryKey (
    PrimaryKey
  , unsafePrimaryKey
  , pkOnColumn
  , pkOnColumns
  , pkName
  , sqlAddPK
  ) where

import Data.Monoid
import qualified Data.Set as S

import DB.SQL

newtype PrimaryKey = PrimaryKey (S.Set RawSQL)
  deriving (Eq, Ord, Show)

unsafePrimaryKey :: S.Set RawSQL -> PrimaryKey
unsafePrimaryKey = PrimaryKey

pkOnColumn :: RawSQL -> Maybe PrimaryKey
pkOnColumn = Just . PrimaryKey . S.singleton

pkOnColumns :: [RawSQL] -> Maybe PrimaryKey
pkOnColumns [] = Nothing
pkOnColumns columns = Just . PrimaryKey . S.fromList $ columns

pkName :: RawSQL -> SQL
pkName tname = mconcat ["pk__", raw tname]

sqlAddPK :: RawSQL -> PrimaryKey -> SQL
sqlAddPK tname (PrimaryKey columns) = "ADD CONSTRAINT"
  <+> pkName tname
  <+> "PRIMARY KEY ("
  <+> intersperseNoWhitespace ", " (map raw . S.toAscList $ columns)
  <+> ")"
