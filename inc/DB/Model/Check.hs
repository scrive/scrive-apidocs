module DB.Model.Check where

import DB.SQL

data TableCheck = TableCheck {
  chkName :: RawSQL
, chkCondition :: RawSQL
} deriving (Eq, Ord, Show)
