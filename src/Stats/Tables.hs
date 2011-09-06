module Stats.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableDocStatEvents :: Table
tableDocStatEvents = Table {
  tblName = "doc_stat_events"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("user_id",     SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("time",        SqlColDesc { colType        = SqlTimestampWithZoneT
                                  , colNullable    = Just False}),
       ("quantity",    SqlColDesc { colType        = SqlSmallIntT
                                  , colNullable    = Just False}),
       ("amount",      SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("document_id", SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE doc_stat_events ("
          ++ "  user_id     BIGINT      NOT NULL"
          ++ ", time        TIMESTAMPTZ NOT NULL"
          ++ ", quantity    SMALLINT    NOT NULL"
          ++ ", amount      INTEGER     NOT NULL"
          ++ ", document_id BIGINT      NOT NULL"
          ++ ", CONSTRAINT pk_doc_stat_events PRIMARY KEY (user_id, quantity, document_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    -- we don't want to delete the stats if a user gets deleted
    -- I don't know if we want to restrict user_id, either
    runRaw conn $ "ALTER TABLE doc_stat_events"
      ++ " ADD CONSTRAINT fk_doc_stat_events_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableDocStatCompanyEvents :: Table
tableDocStatCompanyEvents = Table {
  tblName = "doc_stat_company_events"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [("company_id",  SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("user_id",     SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("time",        SqlColDesc { colType        = SqlTimestampWithZoneT
                                  , colNullable    = Just False}),
       ("quantity",    SqlColDesc { colType        = SqlSmallIntT
                                  , colNullable    = Just False}),
       ("amount",      SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("document_id", SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False})] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE doc_stat_company_events ("
          ++ "  company_id  BIGINT      NOT NULL"
          ++ ", user_id     BIGINT      NOT NULL"
          ++ ", time        TIMESTAMPTZ NOT NULL"
          ++ ", quantity    SMALLINT    NOT NULL"
          ++ ", amount      INTEGER     NOT NULL"
          ++ ", document_id BIGINT      NOT NULL"
          ++ ", CONSTRAINT pk_doc_stat_company_events PRIMARY KEY (company_id, user_id, quantity, document_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    -- we don't want to delete the stats if a user gets deleted
    -- I don't know if we want to restrict user_id, either
    runRaw conn $ "ALTER TABLE doc_stat_company_events"
      ++ " ADD CONSTRAINT fk_doc_stat_company_events_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE doc_stat_company_events"
      ++ " ADD CONSTRAINT fk_doc_stat_company_events_company FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

