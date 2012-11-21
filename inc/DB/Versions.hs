module DB.Versions where

import Database.HDBC
import DB.Functions
import DB.Model
import Data.Monoid ((<>))

tableVersions :: Table
tableVersions = tblTable {
    tblName = "table_versions"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [("name", SqlColDesc { colType = SqlVarCharT, colNullable = Just False }), ("version", SqlColDesc { colType = SqlBigIntT, colNullable = Just False })] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE table_versions ("
          <> "  name TEXT NOT NULL"
          <> ", version INT NOT NULL"
          <> ", CONSTRAINT pk_table_versions PRIMARY KEY (name)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
  }
