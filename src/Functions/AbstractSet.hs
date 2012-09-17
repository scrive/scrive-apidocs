module Functions.AbstractSet where

import DB

-- | This is dummy table that represents current version of SQL functions
-- set we use. It may look fishy to use it like that, but this allows for
-- easy migrations of functions without creating additional machinery for
-- handling them.

abstractFunctionsSet :: Table
abstractFunctionsSet = Table {
    tblName = "abstract_functions_set"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [("dummy", SqlColDesc {colType = SqlBitT, colNullable = Just False})]
        -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE abstract_functions_set ("
          ++ "  dummy BOOLEAN NOT NULL UNIQUE"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
}
