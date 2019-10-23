module Log.Tables where

import DB

logsTables :: [Table]
logsTables = [tableLogs]

tableLogs :: Table
tableLogs = tblTable
  { tblName    = "logs"
  , tblVersion = 3
  , tblColumns =
    [ tblColumn { colName = "insertion_order", colType = BigIntT, colNullable = False }
    , tblColumn { colName     = "insertion_time"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "level", colType = TextT, colNullable = False }
    , tblColumn { colName = "component", colType = TextT, colNullable = False }
    , tblColumn { colName = "message", colType = TextT, colNullable = False }
    , tblColumn { colName = "data", colType = JsonbT, colNullable = False }
    , tblColumn { colName = "domain", colType = ArrayT TextT, colNullable = False }
    ]
  , tblIndexes = [ indexOnColumn "\"time\""
                 , indexOnColumn "component"
                 , indexOnColumn "(data ->> 'document_id'::text)"
                 , indexOnColumn "(data ->> 'file_id'::text)"
                 , indexOnColumn "(data ->> 'signatory_link_id'::text)"
                 , indexOnColumn "(data ->> 'user_id'::text)"
                 ]
  }
