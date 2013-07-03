module ScriveByMail.Tables (
    tableUserMailAPIs
  , tableCompanyMailAPIs
  ) where

import DB

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
