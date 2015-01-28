module DB.Model.Check (
    Check(..)
  , sqlAddCheck
  , sqlDropCheck
  ) where

import Data.Monoid.Space
import Database.PostgreSQL.PQTypes

data Check = Check {
  chkName :: RawSQL ()
, chkCondition :: RawSQL ()
} deriving (Eq, Ord, Show)

sqlAddCheck :: Check -> RawSQL ()
sqlAddCheck Check{..} = smconcat [
    "ADD CONSTRAINT"
  , chkName
  , "CHECK ("
  , chkCondition
  , ")"
  ]

sqlDropCheck :: Check -> RawSQL ()
sqlDropCheck Check{..} = "DROP CONSTRAINT" <+> chkName
