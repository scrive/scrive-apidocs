module Session.Tables where

import DB

tableSessions :: Table
tableSessions = tblTable {
    tblName = "sessions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "pad_user_id", colType = BigIntT }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "csrf_token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "pad_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [tblIndexOnColumn "user_id"]
  }
