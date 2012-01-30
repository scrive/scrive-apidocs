module User.History.Tables where

import Database.HDBC

import DB.Classes
import DB.Model

tableUsersHistory :: Table
tableUsersHistory = Table {
    tblName = "users_history"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> wrapDB $ \conn -> do
    case desc of
      [  ("id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event_type", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("event_data", SqlColDesc {colType = SqlVarCharT, colNullable = Just True})
       , ("ip", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("time", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
       , ("system_version", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
       , ("performing_user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just True})
       ] -> return TVRvalid
      [] -> do
        runRaw conn $ "CREATE TABLE users_history ("
          ++ "  id BIGINT NOT NULL"
          ++ ", user_id BIGINT NOT NULL"
          ++ ", event_type INTEGER NOT NULL"
          ++ ", event_data TEXT NULL "
          ++ ", ip INTEGER NOT NULL"
          ++ ", time TIMESTAMPTZ NOT NULL"
          ++ ", system_version VARCHAR(100) NOT NULL"
          ++ ", performing_user_id BIGINT NULL"
          ++ ", CONSTRAINT pk_users_history PRIMARY KEY (id)"
          ++ ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblPutProperties = wrapDB $ \conn -> do
    runRaw conn "CREATE INDEX idx_users_history_performing_user_id ON users_history(performing_user_id)"
    runRaw conn "CREATE INDEX idx_users_history_time ON users_history(time)"
    runRaw conn "CREATE INDEX idx_users_history_event_type ON users_history(event_type)"
    runRaw conn $ "ALTER TABLE users_history"
      ++ " ADD CONSTRAINT fk_users_history_user_id FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    runRaw conn $ "ALTER TABLE users_history"
      ++ " ADD CONSTRAINT fk_users_history_performing_user_id FOREIGN KEY(performing_user_id)"
      ++ " REFERENCES users(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

