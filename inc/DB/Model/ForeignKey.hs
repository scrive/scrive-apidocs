module DB.Model.ForeignKey (
    ForeignKey(..)
  , ForeignKeyAction(..)
  , fkOnColumn
  , fkOnColumns
  , fkName
  , sqlAddFK
  , sqlDropFK
  ) where

import Data.Monoid
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import qualified Data.Set as S

data ForeignKey = ForeignKey {
  fkColumns    :: S.Set (RawSQL ())
, fkRefTable   :: RawSQL ()
, fkRefColumns :: S.Set (RawSQL ())
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

fkOnColumn :: RawSQL () -> RawSQL () -> RawSQL () -> ForeignKey
fkOnColumn column reftable refcolumn =
  fkOnColumns [column] reftable [refcolumn]

fkOnColumns :: [RawSQL ()] -> RawSQL () -> [RawSQL ()] -> ForeignKey
fkOnColumns columns reftable refcolumns = ForeignKey {
  fkColumns    = S.fromList columns
, fkRefTable   = reftable
, fkRefColumns = S.fromList refcolumns
, fkOnUpdate   = ForeignKeyCascade
, fkOnDelete   = ForeignKeyNoAction
, fkDeferrable = True
, fkDeferred   = False
}

fkName :: RawSQL () -> ForeignKey -> RawSQL ()
fkName tname ForeignKey{..} = mconcat [
    "fk__"
  , tname
  , "__"
  , mintercalate "__" $ S.toAscList fkColumns
  , "__"
  , fkRefTable
  ]

sqlAddFK :: RawSQL () -> ForeignKey -> RawSQL ()
sqlAddFK tname fk@ForeignKey{..} = mconcat [
    "ADD CONSTRAINT" <+> fkName tname fk <+> "FOREIGN KEY ("
  , mintercalate ", " $ S.toAscList fkColumns
  , ") REFERENCES" <+> fkRefTable <+> "("
  , mintercalate ", " $ S.toAscList fkRefColumns
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

sqlDropFK :: RawSQL () -> ForeignKey -> RawSQL ()
sqlDropFK tname fk = "DROP CONSTRAINT" <+> fkName tname fk
