module User.UserAccountRequest.Tables where

import DB

tableUserAccountRequests :: Table
tableUserAccountRequests = tblTable
  { tblName        = "user_account_requests"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "user_id"
  , tblForeignKeys = [fkOnColumn "user_id" "users" "id"]
  }
