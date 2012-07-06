module Company.Tables where

import DB

tableCompanies :: Table
tableCompanies = Table {
    tblName = "companies"
  , tblVersion = 6
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("external_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("service_id", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("name", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("number", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("address", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("zip", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("city", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("country", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("bars_background", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("logo", SqlColDesc {colType = SqlVarBinaryT, colNullable = Just True})
       , ("bars_textcolour", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("email_domain", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE companies ("
          ++ "  id              BIGINT NOT NULL"
          ++ ", external_id     TEXT       NULL"
          ++ ", service_id      TEXT       NULL"
          ++ ", name            TEXT   NOT NULL DEFAULT ''"
          ++ ", number          TEXT   NOT NULL DEFAULT ''"
          ++ ", address         TEXT   NOT NULL DEFAULT ''"
          ++ ", zip             TEXT   NOT NULL DEFAULT ''"
          ++ ", city            TEXT   NOT NULL DEFAULT ''"
          ++ ", country         TEXT   NOT NULL DEFAULT ''"
          ++ ", bars_background TEXT       NULL"
          ++ ", logo            BYTEA      NULL"
          ++ ", bars_textcolour TEXT       NULL"
          ++ ", email_domain    TEXT       NULL"
          ++ ", CONSTRAINT pk_companies PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_companies_service_id ON companies(service_id)"
    kRunRaw "CREATE INDEX idx_companies_external_id ON companies(external_id)"
    kRunRaw $ "ALTER TABLE companies"
      ++ " ADD CONSTRAINT fk_companies_services FOREIGN KEY(service_id)"
      ++ " REFERENCES services(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE SEQUENCE companies_id_seq"
    kRunRaw $ "SELECT setval('companies_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM companies))"
    kRunRaw $ "ALTER TABLE companies ALTER id SET DEFAULT nextval('companies_id_seq')"
  }

