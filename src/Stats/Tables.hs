module Stats.Tables where

import DB

tableDocStatEvents :: Table
tableDocStatEvents = tblTable {
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
          <> "  user_id       BIGINT      NOT NULL"
          <> ", time          TIMESTAMPTZ NOT NULL"
          <> ", quantity      SMALLINT    NOT NULL"
          <> ", amount        INTEGER     NOT NULL"
          <> ", document_id   BIGINT      NOT NULL"
          <> ", company_id    BIGINT          NULL"
          <> ", document_type TEXT        NOT NULL"
          <> ", api_string    TEXT        NOT NULL"
          <> ", CONSTRAINT pk_doc_stat_events PRIMARY KEY (quantity, document_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "company_id" "companies" "id")
                     ]
  }

tableUserStatEvents :: Table
tableUserStatEvents = tblTable {
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
          <> "  user_id       BIGINT      NOT NULL"
          <> ", time          TIMESTAMPTZ NOT NULL"
          <> ", quantity      SMALLINT    NOT NULL"
          <> ", amount        INTEGER     NOT NULL"
          <> ", company_id    BIGINT          NULL"
          <> ", id            BIGINT      NOT NULL      DEFAULT nextval('user_stat_events_id_seq')"
          <> ", CONSTRAINT pk_user_stat_events PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "company_id" "companies" "id")
                     ]
  }

tableSignStatEvents :: Table
tableSignStatEvents = tblTable {
  tblName = "sign_stat_events"
  , tblVersion = 4
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
          <> "  document_id        BIGINT      NOT NULL"
          <> ", signatory_link_id  BIGINT      NOT NULL"
          <> ", time               TIMESTAMPTZ NOT NULL"
          <> ", quantity           SMALLINT    NOT NULL"
          <> ", company_id         BIGINT          NULL"
          <> ", document_process   SMALLINT    NOT NULL"
          <> ", CONSTRAINT pk_sign_stat_events PRIMARY KEY (quantity, document_id, signatory_link_id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "company_id" "companies" "id")
                     , (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     ]
  }
