module ActionQueue.Tables where

import DB

tableAccessNewAccounts :: Table
tableAccessNewAccounts = tblTable {
    tblName = "access_new_accounts"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
    [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
     , ("token", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     ] -> return TVRvalid
    [] -> do
      kRunRaw $ "CREATE TABLE access_new_accounts ("
        <> "  user_id BIGINT NOT NULL"
        <> ", expires TIMESTAMPTZ NOT NULL"
        <> ", token BIGINT NOT NULL"
        <> ", CONSTRAINT pk_access_new_accounts PRIMARY KEY (user_id)"
        <> ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }

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
        <> "  user_id BIGINT NOT NULL"
        <> ", expires TIMESTAMPTZ NOT NULL"
        <> ", remained_emails INTEGER NOT NULL"
        <> ", token BIGINT NOT NULL"
        <> ", CONSTRAINT pk_password_reminders PRIMARY KEY (user_id)"
        <> ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
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
        <> "  user_id BIGINT NOT NULL"
        <> ", expires TIMESTAMPTZ NOT NULL"
        <> ", new_email TEXT NOT NULL"
        <> ", token BIGINT NOT NULL"
        <> ", CONSTRAINT pk_email_change_requests PRIMARY KEY (user_id)"
        <> ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
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
        <> "  user_id BIGINT NOT NULL"
        <> ", expires TIMESTAMPTZ NOT NULL"
        <> ", token BIGINT NOT NULL"
        <> ", CONSTRAINT pk_user_account_requests PRIMARY KEY (user_id)"
        <> ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id") ]
  }
