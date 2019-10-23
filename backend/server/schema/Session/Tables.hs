module Session.Tables where

import DB

tableSessions :: Table
tableSessions = tblTable
  { tblName        = "sessions"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "pad_user_id", colType = BigIntT }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "csrf_token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "domain", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "pad_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes     = [indexOnColumn "user_id", indexOnColumn "pad_user_id"]
  }
