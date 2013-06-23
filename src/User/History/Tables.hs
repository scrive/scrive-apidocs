module User.History.Tables where

import DB

tableUsersHistory :: Table
tableUsersHistory = tblTable {
    tblName = "users_history"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "event_type", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "event_data", colType = TextT }
    , tblColumn { colName = "ip", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "system_version", colType = TextT, colNullable = False }
    , tblColumn { colName = "performing_user_id", colType = BigIntT }
    ]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "performing_user_id" "users" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  , tblIndexes = [tblIndexOnColumn "user_id"]
  }
