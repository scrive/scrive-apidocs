module DB.Model.Check (
    TableCheck(..)
  , sqlAddCheck
  , sqlDropCheck
  ) where

import Data.Monoid.Space
import Database.PostgreSQL.PQTypes

data TableCheck = TableCheck {
  chkName :: RawSQL ()
, chkCondition :: RawSQL ()
} deriving (Eq, Ord, Show)

sqlAddCheck :: TableCheck -> RawSQL ()
sqlAddCheck TableCheck{..} = smconcat [
    "ADD CONSTRAINT"
  , chkName
  , "CHECK ("
  , chkCondition
  , ")"
  ]

sqlDropCheck :: TableCheck -> RawSQL ()
sqlDropCheck TableCheck{..} = "DROP CONSTRAINT" <+> chkName
