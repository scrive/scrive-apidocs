module User.History.Tables where

import DB

tableUsersHistory :: Table
tableUsersHistory = tblTable {
    tblName = "users_history"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event_type", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event_data", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("system_version", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("performing_user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE users_history ("
          <> "  user_id BIGINT NOT NULL"
          <> ", event_type INTEGER NOT NULL"
          <> ", event_data TEXT NULL "
          <> ", ip INTEGER NOT NULL"
          <> ", time TIMESTAMPTZ NOT NULL"
          <> ", system_version TEXT NOT NULL"
          <> ", performing_user_id BIGINT NULL"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblIndexes = [ tblIndexOnColumn "user_id"
                 ]
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE users_history"
      <> " ADD CONSTRAINT fk_users_history_user_id FOREIGN KEY(user_id)"
      <> " REFERENCES users(id) ON DELETE CASCADE ON UPDATE CASCADE"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "ALTER TABLE users_history"
      <> " ADD CONSTRAINT fk_users_history_performing_user_id FOREIGN KEY(performing_user_id)"
      <> " REFERENCES users(id) ON DELETE SET NULL ON UPDATE CASCADE"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
  }
