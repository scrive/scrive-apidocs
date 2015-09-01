module Log.Migrations (logsMigrations) where

import DB
import DB.Checks
import KontraPrelude
import Log.Tables

logsMigrations :: MonadDB m => [Migration m]
logsMigrations = [
    createLogsTable
  , addColumnDomain
  , addObjectIndexes
  ]

addObjectIndexes :: MonadDB m => Migration m
addObjectIndexes = Migration {
  mgrTable = tableLogs
, mgrFrom = 2
, mgrDo = do
  runSQL_ "DROP INDEX idx__logs__time"
  mapM_ (runQuery_ . sqlCreateIndex (tblName tableLogs) . indexOnColumn) [
      "\"time\""
    , "(data ->> 'document_id'::text)"
    , "(data ->> 'file_id'::text)"
    , "(data ->> 'signatory_link_id'::text)"
    , "(data ->> 'user_id'::text)"
    ]
}

addColumnDomain :: MonadDB m => Migration m
addColumnDomain = Migration {
  mgrTable = tableLogs
, mgrFrom = 1
, mgrDo = do
  let tname = tblName tableLogs
  runQuery_ $ sqlCreateIndex tname $ indexOnColumn "component"
  runQuery_ $ sqlAlterTable tname [sqlAddColumn tblColumn {
    colName = "domain"
  , colType = ArrayT TextT
  , colNullable = False
  , colDefault = Just "ARRAY[]::text[]"
  }]
  runQuery_ $ sqlAlterTable tname [sqlAlterColumn "domain" "DROP DEFAULT"]
}

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
