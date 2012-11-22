module Mails.Tables (
    mailerTables
  , tableMails
  , tableMailEvents
  ) where

import DB

mailerTables :: [Table]
mailerTables = [
    tableMails
  , tableMailEvents
  ]

tableMails :: Table
tableMails = tblTable {
    tblName = "mails"
  , tblVersion = 2
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("sender", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("receivers", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("title", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("content", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("attachments", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("x_smtp_attrs", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("to_be_sent", SqlColDesc { colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("sent", SqlColDesc { colType = SqlTimestampWithZoneT, colNullable = Just True})
       , ("service_test" , SqlColDesc { colType = SqlBitT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mails ("
          ++ "  id BIGSERIAL NOT NULL"
          ++ ", token BIGINT NOT NULL"
          ++ ", sender TEXT NOT NULL"
          ++ ", receivers TEXT NOT NULL"
          ++ ", title TEXT NULL"
          ++ ", content TEXT NULL"
          ++ ", attachments TEXT NULL"
          ++ ", x_smtp_attrs TEXT NULL"
          ++ ", to_be_sent TIMESTAMPTZ NOT NULL"
          ++ ", sent TIMESTAMPTZ NULL"
          ++ ", service_test BOOL NOT NULL"
          ++ ", CONSTRAINT pk_mails PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = return ()
  }

tableMailEvents :: Table
tableMailEvents = tblTable {
    tblName = "mail_events"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("mail_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("event_read", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE mail_events ("
          ++ "  id BIGSERIAL NOT NULL"
          ++ ", mail_id BIGINT NOT NULL"
          ++ ", event TEXT NOT NULL"
          ++ ", event_read TIMESTAMPTZ NULL"
          ++ ", CONSTRAINT pk_mail_events PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw "CREATE INDEX idx_mail_events_mail_id ON mail_events(mail_id)"
    kRunRaw $ "ALTER TABLE mail_events"
      ++ " ADD CONSTRAINT fk_mail_events_mails FOREIGN KEY(mail_id)"
      ++ " REFERENCES mails(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
