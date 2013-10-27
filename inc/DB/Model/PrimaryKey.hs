module DB.Model.PrimaryKey where

import Data.Monoid

import DB.SQL

pkName :: RawSQL -> SQL
pkName tname = mconcat ["pk__", raw tname]

sqlAddPK :: RawSQL -> [RawSQL] -> SQL
sqlAddPK tname columns = "ADD CONSTRAINT"
  <+> pkName tname
  <+> "PRIMARY KEY ("
  <+> intersperseNoWhitespace ", " (map raw columns)
  <+> ")"
