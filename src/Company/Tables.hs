module Company.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableCompanies :: Table
tableCompanies = Table {
    tblName = "companies"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("external_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("address", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("zip", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("city", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("country", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE companies ("
          ++ "  id BIGINT NOT NULL"
          ++ ", external_id TEXT NULL"
          ++ ", service_id TEXT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", number TEXT NOT NULL"
          ++ ", address TEXT NOT NULL"
          ++ ", zip TEXT NOT NULL"
          ++ ", city TEXT NOT NULL"
          ++ ", country TEXT NOT NULL"
          ++ ", CONSTRAINT pk_companies PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn "CREATE INDEX idx_companies_service_id ON companies(service_id)"
    runRaw conn "CREATE INDEX idx_companies_external_id ON companies(external_id)"
    runRaw conn $ "ALTER TABLE companies"
      ++ " ADD CONSTRAINT fk_companies_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableCompanyInvites :: Table
tableCompanyInvites = Table {
    tblName = "companyinvites"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [ ("email", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("first_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("last_name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE companyinvites ("
          ++ "  email TEXT NOT NULL"
          ++ ", first_name TEXT NOT NULL"
          ++ ", last_name TEXT NOT NULL"
          ++ ", company_id BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_companyinvites PRIMARY KEY (email, company_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn "CREATE INDEX idx_users_company_id ON companyinvites(company_id)"
    runRaw conn "CREATE INDEX idx_users_email ON companyinvites(email)"
    runRaw conn $ "ALTER TABLE companyinvites"
      ++ " ADD CONSTRAINT fk_companyinvites_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }