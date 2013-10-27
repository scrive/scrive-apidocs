module DB.Model.Check where

import Data.Monoid

import DB.SQL

data TableCheck = TableCheck {
  chkName :: RawSQL
, chkCondition :: RawSQL
} deriving (Eq, Ord, Show)

checkName :: RawSQL -> TableCheck -> SQL
checkName tname TableCheck{chkName} = mconcat [
    "check__"
  , raw tname
  , "__"
  , raw chkName
  ]

sqlAddCheck :: RawSQL -> TableCheck -> SQL
sqlAddCheck tname chk@TableCheck{..} = "ADD CONSTRAINT"
  <+> checkName tname chk
  <+> "CHECK ("
  <+> raw chkCondition
  <+> ")"
