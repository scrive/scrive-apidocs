module ScriveByMail.Tables 
       (
         tableMailAPIDelay,
         tableUserMailAPIs,
         tableCompanyMailAPIs
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
      [  ("id",             SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("key",            SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("email",          SqlColDesc {colType = SqlVarCharT,  colNullable = Just False})
       , ("email_text",     SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("time",           SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("expires",        SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("status",         SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("company_id",     SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mail_api_delay ("
          ++ "  id          BIGSERIAL NOT NULL"
          ++ ", key            BIGINT NOT NULL"
          ++ ", email            TEXT NOT NULL"
          ++ ", email_text       TEXT NOT NULL"
          ++ ", time      TIMESTAMPTZ NOT NULL"
          ++ ", expires   TIMESTAMPTZ NOT NULL"
          ++ ", status       SMALLINT NOT NULL"
          ++ ", company_id     BIGINT NOT NULL"
          ++ ", CONSTRAINT pk_mail_api_delay PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE mail_api_delay"
      ++ " ADD CONSTRAINT fk_mail_api_delay FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableUserMailAPIs :: Table
tableUserMailAPIs = Table {
    tblName = "user_mail_apis"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("key", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("daily_limit", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sent_today", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("last_sent_date", SqlColDesc {colType = SqlDateT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE user_mail_apis ("
          ++ "  user_id BIGINT NOT NULL"
          ++ ", key BIGINT NOT NULL"
          ++ ", daily_limit INTEGER NOT NULL"
          ++ ", sent_today INTEGER NOT NULL"
          ++ ", last_sent_date DATE NOT NULL"
          ++ ", CONSTRAINT pk_user_mail_apis PRIMARY KEY (user_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE user_mail_apis"
      ++ " ADD CONSTRAINT fk_user_mail_apis_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableCompanyMailAPIs :: Table
tableCompanyMailAPIs = Table {
    tblName = "company_mail_apis"
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
