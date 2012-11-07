{-# LANGUAGE OverloadedStrings #-}
module Stats.Tables where

import DB

tableDocStatEvents :: Table
tableDocStatEvents = Table {
  tblName = "doc_stat_events"
  , tblVersion = 5
  , tblCreateOrValidate = \desc -> case desc of
      [("user_id",     SqlColDesc { colType       = SqlBigIntT
                                  , colNullable   = Just False}),
       ("time",        SqlColDesc { colType       = SqlTimestampWithZoneT
                                  , colNullable   = Just False}),
       ("quantity",    SqlColDesc { colType       = SqlSmallIntT
                                  , colNullable   = Just False}),
       ("amount",      SqlColDesc { colType       = SqlBigIntT
                                  , colNullable   = Just False}),
       ("document_id", SqlColDesc { colType       = SqlBigIntT
                                  , colNullable   = Just False}),
       ("company_id",  SqlColDesc { colType       = SqlBigIntT
                                  , colNullable   = Just True}),
       ("document_type", SqlColDesc { colType     = SqlVarCharT
                                    , colNullable = Just False}),
       ("api_string", SqlColDesc { colType        = SqlVarCharT
                                 , colNullable    = Just False})
                                    ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE doc_stat_events ("
          ++ "  user_id       BIGINT      NOT NULL"
          ++ ", time          TIMESTAMPTZ NOT NULL"
          ++ ", quantity      SMALLINT    NOT NULL"
          ++ ", amount        INTEGER     NOT NULL"
          ++ ", document_id   BIGINT      NOT NULL"
          ++ ", company_id    BIGINT          NULL"
          ++ ", document_type TEXT        NOT NULL"
          ++ ", api_string    TEXT        NOT NULL"
          ++ ", CONSTRAINT pk_doc_stat_events PRIMARY KEY (quantity, document_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    -- we don't want to delete the stats if a user gets deleted
    -- I don't know if we want to restrict user_id, either
    kRunRaw $ "ALTER TABLE doc_stat_events"
      ++ " ADD CONSTRAINT fk_doc_stat_events_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE doc_stat_events"
      ++ " ADD CONSTRAINT fk_doc_stat_events_company FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableUserStatEvents :: Table
tableUserStatEvents = Table {
  tblName = "user_stat_events"
  , tblVersion = 4
  , tblCreateOrValidate = \desc -> case desc of
      [("user_id",     SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("time",        SqlColDesc { colType        = SqlTimestampWithZoneT
                                  , colNullable    = Just False}),
       ("quantity",    SqlColDesc { colType        = SqlSmallIntT
                                  , colNullable    = Just False}),
       ("amount",      SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False}),
       ("company_id",  SqlColDesc { colType = SqlBigIntT
                                 , colNullable = Just True}),
       ("id",          SqlColDesc { colType        = SqlBigIntT
                                  , colNullable    = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE SEQUENCE user_stat_events_id_seq"
        kRunRaw $ "CREATE TABLE user_stat_events ("
          ++ "  user_id       BIGINT      NOT NULL"
          ++ ", time          TIMESTAMPTZ NOT NULL"
          ++ ", quantity      SMALLINT    NOT NULL"
          ++ ", amount        INTEGER     NOT NULL"
          ++ ", company_id    BIGINT          NULL"
          ++ ", id            BIGINT      NOT NULL      DEFAULT nextval('user_stat_events_id_seq')"
          ++ ", CONSTRAINT pk_user_stat_events PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_user_stat_events_user_id ON user_stat_events(user_id)"
    -- we don't want to delete the stats if a user gets deleted
    kRunRaw $ "ALTER TABLE user_stat_events"
      ++ " ADD CONSTRAINT fk_user_stat_events_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE user_stat_events"
      ++ " ADD CONSTRAINT fk_user_stat_events_company FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableSignStatEvents :: Table
tableSignStatEvents = Table {
  tblName = "sign_stat_events"
  , tblVersion = 3
  , tblCreateOrValidate = \desc -> case desc of
      [("document_id",       SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just False}),
       ("signatory_link_id", SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just False}),
       ("time",              SqlColDesc { colType     = SqlTimestampWithZoneT
                                        , colNullable = Just False}),
       ("quantity",          SqlColDesc { colType     = SqlSmallIntT
                                        , colNullable = Just False}),
       ("company_id",        SqlColDesc { colType     = SqlBigIntT
                                        , colNullable = Just True}),
       ("document_process",  SqlColDesc { colType     = SqlSmallIntT
                                        , colNullable = Just False})] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE sign_stat_events ("
          ++ "  document_id        BIGINT      NOT NULL"          
          ++ ", signatory_link_id  BIGINT      NOT NULL"
          ++ ", time               TIMESTAMPTZ NOT NULL"
          ++ ", quantity           SMALLINT    NOT NULL"
          ++ ", company_id         BIGINT          NULL"
          ++ ", document_process   SMALLINT    NOT NULL"
          ++ ", CONSTRAINT pk_sign_stat_events PRIMARY KEY (quantity, document_id, signatory_link_id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    {-
     -- I wanted to add this but apparently signatory_link_id is not unique, so I could not.
    kRunRaw $ "ALTER TABLE sign_stat_events"
      ++ " ADD CONSTRAINT fk_sign_stat_events_signatory_link FOREIGN KEY(signatory_link_id)"
      ++ " REFERENCES signatory_links(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    -}
    kRunRaw $ "ALTER TABLE sign_stat_events"
      ++ " ADD CONSTRAINT fk_sign_stat_events_documents FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE sign_stat_events"
      ++ " ADD CONSTRAINT fk_sign_stat_events_company FOREIGN KEY(company_id)"
      ++ " REFERENCES companies(id) ON UPDATE RESTRICT ON DELETE NO ACTION"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
