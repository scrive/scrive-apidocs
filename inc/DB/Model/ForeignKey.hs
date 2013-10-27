module DB.Model.ForeignKey where

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

tblForeignKeyColumn :: RawSQL -> RawSQL -> RawSQL -> ForeignKey
tblForeignKeyColumn column reftable refcolumn =
  tblForeignKeyColumns [column] reftable [refcolumn]

tblForeignKeyColumns :: [RawSQL] -> RawSQL -> [RawSQL] -> ForeignKey
tblForeignKeyColumns columns reftable refcolumns = ForeignKey {
  fkColumns    = S.fromList columns
, fkRefTable   = reftable
, fkRefColumns = S.fromList refcolumns
, fkOnUpdate   = ForeignKeyCascade
, fkOnDelete   = ForeignKeyNoAction
, fkDeferrable = True
, fkDeferred   = False
}
