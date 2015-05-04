module Log.Migrations (logsMigrations) where

import DB
import DB.Checks
import KontraPrelude
import Log.Tables

logsMigrations :: MonadDB m => [Migration m]
logsMigrations = [
    createLogsTable
  ]

createLogsTable :: MonadDB m => Migration m
createLogsTable = Migration {
  mgrTable = tableLogs
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "logs"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "insertion_order", colType = BigIntT,  colNullable = False }
    , tblColumn { colName = "insertion_time", colType = TimestampWithZoneT,  colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "level", colType = TextT, colNullable = False }
    , tblColumn { colName = "component", colType = TextT, colNullable = False }
    , tblColumn { colName = "message", colType = TextT, colNullable = False }
    , tblColumn { colName = "data", colType = JsonbT, colNullable = False }
    ]
  , tblIndexes = [
        indexOnColumn "time"
      ]
  }
}
