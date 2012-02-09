module CompanyAccounts.Tables (
    tableCompanyInvites
  ) where

import Database.HDBC

import DB.Classes
import DB.Model


tableCompanyInvites :: Table
tableCompanyInvites = Table {
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
          ++ "  email TEXT NOT NULL"
          ++ ", first_name TEXT NOT NULL"
          ++ ", last_name TEXT NOT NULL"
          ++ ", company_id BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_companyinvites PRIMARY KEY (email, company_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_companyinvites_id ON companyinvites(company_id)"
    kRunRaw "CREATE INDEX idx_companyinvites_email ON companyinvites(email)"
    kRunRaw $ "ALTER TABLE companyinvites"
      ++ " ADD CONSTRAINT fk_companyinvites_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
