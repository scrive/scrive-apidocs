module Log.Tables where

import DB
import KontraPrelude

logsTables :: [Table]
logsTables = [
    tableLogs
  ]

tableLogs :: Table
tableLogs = tblTable {
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
