module ActionQueue.Tables where

import DB

tablePasswordReminders :: Table
tablePasswordReminders = tblTable {
    tblName = "password_reminders"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
    [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
     , ("remained_emails", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     ] -> return TVRvalid
    [] -> do
      kRunRaw $ "CREATE TABLE password_reminders ("
        ++ "  user_id BIGINT NOT NULL"
        ++ ", expires TIMESTAMPTZ NOT NULL"
        ++ ", remained_emails INTEGER NOT NULL"
        ++ ", token BIGINT NOT NULL"
        ++ ", CONSTRAINT pk_password_reminders PRIMARY KEY (user_id)"
        ++ ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE password_reminders"
      ++ " ADD CONSTRAINT fk_password_reminders_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableEmailChangeRequests :: Table
tableEmailChangeRequests = tblTable {
    tblName = "email_change_requests"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
    [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
     , ("new_email", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
     , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     ] -> return TVRvalid
    [] -> do
      kRunRaw $ "CREATE TABLE email_change_requests ("
        ++ "  user_id BIGINT NOT NULL"
        ++ ", expires TIMESTAMPTZ NOT NULL"
        ++ ", new_email TEXT NOT NULL"
        ++ ", token BIGINT NOT NULL"
        ++ ", CONSTRAINT pk_email_change_requests PRIMARY KEY (user_id)"
        ++ ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE email_change_requests"
      ++ " ADD CONSTRAINT fk_email_change_requests_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

tableUserAccountRequests :: Table
tableUserAccountRequests = tblTable {
    tblName = "user_account_requests"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
    [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
     , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     ] -> return TVRvalid
    [] -> do
      kRunRaw $ "CREATE TABLE user_account_requests ("
        ++ "  user_id BIGINT NOT NULL"
        ++ ", expires TIMESTAMPTZ NOT NULL"
        ++ ", token BIGINT NOT NULL"
        ++ ", CONSTRAINT pk_user_account_requests PRIMARY KEY (user_id)"
        ++ ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblPutProperties = do
    kRunRaw $ "ALTER TABLE user_account_requests"
      ++ " ADD CONSTRAINT fk_user_account_requests_users FOREIGN KEY(user_id)"
      ++ " REFERENCES users(id) ON DELETE RESTRICT ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }
