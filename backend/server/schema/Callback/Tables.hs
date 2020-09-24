module Callback.Tables where

import DB

tableCallbackConsumers :: Table
tableCallbackConsumers = tblTable
  { tblName       = "callback_consumers"
  , tblVersion    = 1
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName     = "last_activity"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableCallbacks :: Table
tableCallbacks = tblTable
  { tblName        = "callbacks"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName     = "attempts"
                , colType     = IntegerT
                , colNullable = False
                , colDefault  = Just "0"
                }
    , tblColumn { colName = "url", colType = TextT, colNullable = False }
    , tblColumn { colName = "payload", colType = JsonbT, colNullable = False }
    , tblColumn { colName = "auth_method", colType = JsonbT, colNullable = False }
    , tblColumn { colName = "next", colType = BigIntT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "next" "callbacks" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "callback_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                           }
    ]
  , tblIndexes     = [indexOnColumn "next"]
  }
