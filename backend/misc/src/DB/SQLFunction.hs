module DB.SQLFunction where

import Database.PostgreSQL.PQTypes

import KontraPrelude

-- | Basic SQL functions handling

newtype SQLFunction = SQLFunction { sqlFunDef :: RawSQL () }

defineFunctions :: MonadDB m => [SQLFunction]-> m ()
defineFunctions = mapM_ $ runQuery_ . sqlFunDef
