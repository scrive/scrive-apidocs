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
      [("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False}), ("external_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True}), ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE companies ("
          ++ "  id BIGINT NOT NULL"
          ++ ", external_id TEXT NULL"
          ++ ", service_id TEXT NULL"
          ++ ", CONSTRAINT pk_companies PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn "CREATE INDEX idx_companies_service_id ON companies(service_id)"
    runRaw conn "CREATE INDEX idx_companies_external_id ON companies(external_id)"
    runRaw conn $ "ALTER TABLE companies"
      ++ " ADD CONSTRAINT fk_companies_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE CASCADE ON UPDATE CASCADE"
  }

tableCompanyInfos :: Table
tableCompanyInfos = Table {
    tblName = "company_infos"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("company_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False}), ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("address", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("zip", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("city", SqlColDesc {colType = SqlVarCharT, colNullable = Just False}), ("country", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE company_infos ("
          ++ "  company_id BIGINT NOT NULL"
          ++ ", name TEXT NOT NULL"
          ++ ", number TEXT NOT NULL"
          ++ ", address TEXT NOT NULL"
          ++ ", zip TEXT NOT NULL"
          ++ ", city TEXT NOT NULL"
          ++ ", country TEXT NOT NULL"
          ++ ", CONSTRAINT pk_company_infos PRIMARY KEY (company_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn $ "ALTER TABLE company_infos"
      ++ " ADD CONSTRAINT fk_company_infos_companies FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE CASCADE ON UPDATE CASCADE"
  }
