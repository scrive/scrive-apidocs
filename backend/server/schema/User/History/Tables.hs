module User.History.Tables where

import DB
import KontraPrelude

tableUsersHistory :: Table
tableUsersHistory = tblTable {
    tblName = "users_history"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "event_type", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "event_data", colType = TextT }
    , tblColumn { colName = "ip", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "system_version", colType = TextT, colNullable = False }
    , tblColumn { colName = "performing_user_id", colType = BigIntT }
    , tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "performing_user_id" "users" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  , tblIndexes = [
      indexOnColumn "user_id"
    , indexOnColumn "performing_user_id"
    ]
  }
