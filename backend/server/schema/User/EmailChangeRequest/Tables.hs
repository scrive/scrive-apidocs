module User.EmailChangeRequest.Tables where

import DB

tableEmailChangeRequests :: Table
tableEmailChangeRequests = tblTable {
    tblName = "email_change_requests"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "new_email", colType = TextT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "user_id"
  , tblForeignKeys = [
      (fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
