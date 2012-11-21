module Session.Tables where

import DB

tableSessions :: Table
tableSessions = tblTable {
    tblName = "sessions"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("pad_user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("csrf_token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE sessions ("
          <> "  id BIGSERIAL NOT NULL"
          <> ", user_id BIGINT NULL"
          <> ", pad_user_id BIGINT NULL"
          <> ", expires TIMESTAMPTZ NOT NULL"
          <> ", token BIGINT NOT NULL"
          <> ", csrf_token BIGINT NOT NULL"
          <> ", CONSTRAINT pk_sessions PRIMARY KEY (id)"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "CREATE INDEX idx_sessions_user_id ON sessions(user_id)"
    kRunRaw $ "ALTER TABLE sessions"
      <> " ADD CONSTRAINT fk_sessions_user_id FOREIGN KEY(user_id)"
      <> " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE sessions"
      <> " ADD CONSTRAINT fk_sessions_pad_user_id FOREIGN KEY(pad_user_id)"
      <> " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
}
