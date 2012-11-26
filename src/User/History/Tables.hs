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
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "performing_user_id" "users" "id")
                       { fkOnDelete = ForeignKeySetNull } ]
  }
