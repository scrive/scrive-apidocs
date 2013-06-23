module ActionQueue.Tables where

import DB

tableAccessNewAccounts :: Table
tableAccessNewAccounts = tblTable {
    tblName = "access_new_accounts"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tablePasswordReminders :: Table
tablePasswordReminders = tblTable {
    tblName = "password_reminders"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "remained_emails", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableEmailChangeRequests :: Table
tableEmailChangeRequests = tblTable {
    tblName = "email_change_requests"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "new_email", colType = TextT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableUserAccountRequests :: Table
tableUserAccountRequests = tblTable {
    tblName = "user_account_requests"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [tblForeignKeyColumn "user_id" "users" "id"]
  }
