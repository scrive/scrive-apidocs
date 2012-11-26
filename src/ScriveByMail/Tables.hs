module ScriveByMail.Tables (
    tableMailAPIDelay
  , tableUserMailAPIs
  , tableUserRequest
  , tableCompanyMailAPIs
  ) where

import DB

tableUserRequest :: Table
tableUserRequest = tblTable  {
    tblName = "mail_api_user_request"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",             SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("key",            SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("email",          SqlColDesc {colType = SqlVarCharT,  colNullable = Just False})
       , ("time",           SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("expires",        SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("status",         SqlColDesc {colType = SqlSmallIntT, colNullable = Just False})
       , ("company_id",     SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mail_api_user_request ("
          <> "  id          BIGSERIAL NOT NULL"
          <> ", key            BIGINT NOT NULL"
          <> ", email            TEXT NOT NULL"
          <> ", time      TIMESTAMPTZ NOT NULL"
          <> ", expires   TIMESTAMPTZ NOT NULL"
          <> ", status       SMALLINT NOT NULL"
          <> ", company_id     BIGINT NOT NULL"
          <> ", CONSTRAINT pk_mail_api_user_request PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "company_id" "companies" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableMailAPIDelay :: Table
tableMailAPIDelay = tblTable {
    tblName = "mail_api_delay"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",              SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       , ("email_text",      SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("user_request_id", SqlColDesc {colType = SqlBigIntT,  colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mail_api_delay ("
          <> "  id           BIGSERIAL NOT NULL"
          <> ", email_text        TEXT NOT NULL"
          <> ", user_request_id BIGINT NOT NULL"
          <> ", CONSTRAINT pk_mail_api_delay PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_request_id" "mail_api_user_request" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableUserMailAPIs :: Table
tableUserMailAPIs = tblTable {
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
          <> "  user_id BIGINT NOT NULL"
          <> ", key BIGINT NOT NULL"
          <> ", daily_limit INTEGER NOT NULL"
          <> ", sent_today INTEGER NOT NULL"
          <> ", last_sent_date DATE NOT NULL"
          <> ", CONSTRAINT pk_user_mail_apis PRIMARY KEY (user_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

tableCompanyMailAPIs :: Table
tableCompanyMailAPIs = tblTable {
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
          <> "  company_id     BIGINT NOT NULL"
          <> ", key            BIGINT NOT NULL"
          <> ", daily_limit   INTEGER NOT NULL"
          <> ", sent_today    INTEGER NOT NULL"
          <> ", last_sent_date   DATE NOT NULL"
          <> ", CONSTRAINT pk_company_mail_apis PRIMARY KEY (company_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "company_id" "companies" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }
