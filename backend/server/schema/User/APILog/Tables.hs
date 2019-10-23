module User.APILog.Tables (tableAPILogs) where

import DB

tableAPILogs :: Table
tableAPILogs = tblTable
  { tblName       = "api_call_logs"
  , tblVersion    = 2
  , tblColumns    =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "request_uri", colType = TextT, colNullable = False }
    , tblColumn { colName = "request_method", colType = TextT, colNullable = False }
    , tblColumn { colName = "request_params_get", colType = JsonT, colNullable = False }
    , tblColumn { colName = "request_params_post", colType = JsonT, colNullable = False }
    , tblColumn { colName = "response_code", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "response_body", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes    = [indexOnColumn "user_id"]
  }
