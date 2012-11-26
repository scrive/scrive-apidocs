module CompanyAccounts.Tables (
    tableCompanyInvites
  ) where

import DB

tableCompanyInvites :: Table
tableCompanyInvites = tblTable {
    tblName = "companyinvites"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [ ("email", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("first_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("last_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE companyinvites ("
          <> "  email TEXT NOT NULL"
          <> ", first_name TEXT NOT NULL"
          <> ", last_name TEXT NOT NULL"
          <> ", company_id BIGINT NOT NULL"
          <> ", CONSTRAINT pk_companyinvites PRIMARY KEY (email, company_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "company_id"
                 , tblIndexOnColumn "email" ]
  , tblForeignKeys = [ (tblForeignKeyColumn "company_id" "companies" "id") ]
  }
