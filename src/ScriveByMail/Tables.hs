module ScriveByMail.Tables 
       (
         tableMailAPIDelay
       )
    where

import Database.HDBC

import DB.Classes
import DB.Model

tableMailAPIDelay :: Table
tableMailAPIDelay = Table {
    tblName = "mail_api_delay"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("company_id",     SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("key",            SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("daily_limit",    SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sent_today",     SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("last_sent_date", SqlColDesc {colType = SqlDateT,   colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE company_mail_apis ("
          ++ "  company_id     BIGINT NOT NULL"
          ++ ", key            BIGINT NOT NULL"
          ++ ", daily_limit   INTEGER NOT NULL"
          ++ ", sent_today    INTEGER NOT NULL"
          ++ ", last_sent_date   DATE NOT NULL"
          ++ ", CONSTRAINT pk_company_mail_apis PRIMARY KEY (company_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE company_mail_apis"
      ++ " ADD CONSTRAINT fk_company_mail_apis_users FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
