module Attachment.Tables where

import DB

tableAttachments :: Table
tableAttachments = tblTable {
    tblName = "attachments"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id",      SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("title",   SqlColDesc {colType = SqlVarCharT,           colNullable = Just False})
       , ("ctime",   SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("mtime",   SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("user_id", SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("file_id", SqlColDesc {colType = SqlBigIntT,            colNullable = Just False})
       , ("shared",  SqlColDesc {colType = SqlBitT,               colNullable = Just False})
       , ("deleted", SqlColDesc {colType = SqlBitT,               colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE attachments"
          <> "( id         BIGSERIAL   NOT NULL"
          <> ", title      TEXT        NOT NULL"
          <> ", ctime      TIMESTAMPTZ NOT NULL"
          <> ", mtime      TIMESTAMPTZ NOT NULL"
          <> ", user_id    BIGINT      NOT NULL"
          <> ", file_id    BIGINT      NOT NULL"
          <> ", shared     BOOL        NOT NULL"
          <> ", deleted    BOOL        NOT NULL"
          <> ", CONSTRAINT pk_attachments PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE attachments"
      <> " ADD CONSTRAINT fk_attachments_user_id FOREIGN KEY(user_id)"
      <> " REFERENCES users(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    return ()
  }
