module DB.Model.Check where

import DB.SQL

data TableCheck = TableCheck {
  chkName :: RawSQL
, chkCondition :: RawSQL
} deriving (Eq, Ord, Show)

sqlAddCheck :: TableCheck -> SQL
sqlAddCheck TableCheck{..} = "ADD CONSTRAINT"
  <+> raw chkName
  <+> "CHECK ("
  <+> raw chkCondition
  <+> ")"

sqlDropCheck :: TableCheck -> SQL
sqlDropCheck TableCheck{..} = "DROP CONSTRAINT" <+> raw chkName
