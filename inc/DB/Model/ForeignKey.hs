module DB.Model.ForeignKey where

import Data.Monoid
import qualified Data.Set as S

import DB.SQL

data ForeignKey = ForeignKey {
  fkColumns    :: S.Set RawSQL
, fkRefTable   :: RawSQL
, fkRefColumns :: S.Set RawSQL
, fkOnUpdate   :: ForeignKeyAction
, fkOnDelete   :: ForeignKeyAction
, fkDeferrable :: Bool
, fkDeferred   :: Bool
} deriving (Eq, Ord, Show)

data ForeignKeyAction
  = ForeignKeyNoAction
  | ForeignKeyRestrict
  | ForeignKeyCascade
  | ForeignKeySetNull
  | ForeignKeySetDefault
  deriving (Eq, Ord, Show)

fkOnColumn :: RawSQL -> RawSQL -> RawSQL -> ForeignKey
fkOnColumn column reftable refcolumn =
  fkOnColumns [column] reftable [refcolumn]

fkOnColumns :: [RawSQL] -> RawSQL -> [RawSQL] -> ForeignKey
fkOnColumns columns reftable refcolumns = ForeignKey {
  fkColumns    = S.fromList columns
, fkRefTable   = reftable
, fkRefColumns = S.fromList refcolumns
, fkOnUpdate   = ForeignKeyCascade
, fkOnDelete   = ForeignKeyNoAction
, fkDeferrable = True
, fkDeferred   = False
}

fkName :: RawSQL -> ForeignKey -> SQL
fkName tname ForeignKey{..} = mconcat [
    "fk__"
  , raw tname
  , "__"
  , intersperseNoWhitespace "__" (map raw . S.toAscList $ fkColumns)
  , "__"
  , raw fkRefTable
  ]

sqlAddFK :: RawSQL -> ForeignKey -> SQL
sqlAddFK tname fk@ForeignKey{..} = mconcat [
    "ADD CONSTRAINT" <+> fkName tname fk <+> "FOREIGN KEY ("
  , intersperseNoWhitespace ", " (map raw . S.toAscList $ fkColumns)
  , ") REFERENCES" <+> raw fkRefTable <+> "("
  , intersperseNoWhitespace ", " (map raw . S.toAscList $ fkRefColumns)
  , ") ON UPDATE" <+> foreignKeyActionToSQL fkOnUpdate
  , "  ON DELETE" <+> foreignKeyActionToSQL fkOnDelete
  , " " <> if fkDeferrable then "DEFERRABLE" else "NOT DEFERRABLE"
  , " INITIALLY" <+> if fkDeferred then "DEFERRED" else "IMMEDIATE"
  ]
  where
    foreignKeyActionToSQL ForeignKeyNoAction = "NO ACTION"
    foreignKeyActionToSQL ForeignKeyRestrict = "RESTRICT"
    foreignKeyActionToSQL ForeignKeyCascade = "CASCADE"
    foreignKeyActionToSQL ForeignKeySetNull = "SET NULL"
    foreignKeyActionToSQL ForeignKeySetDefault = "SET DEFAULT"

sqlDropFK :: RawSQL -> ForeignKey -> SQL
sqlDropFK tname fk = "DROP CONSTRAINT" <+> fkName tname fk
