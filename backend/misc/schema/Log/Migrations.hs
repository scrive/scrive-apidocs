module Log.Migrations (logsMigrations) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import Log.Tables

logsMigrations :: MonadDB m => [Migration m]
logsMigrations = [createLogsTable, addColumnDomain, addObjectIndexes]

addObjectIndexes :: MonadDB m => Migration m
addObjectIndexes = Migration
  { mgrTableName = tblName tableLogs
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runSQL_ "ALTER INDEX idx__logs__time RENAME TO idx__logs__$time$"
                     mapM_
                       ( runQuery_
                       . sqlCreateIndexSequentially (tblName tableLogs)
                       . indexOnColumn
                       )
                       [ "(data ->> 'document_id'::text)"
                       , "(data ->> 'file_id'::text)"
                       , "(data ->> 'signatory_link_id'::text)"
                       , "(data ->> 'user_id'::text)"
                       ]
  }

addColumnDomain :: MonadDB m => Migration m
addColumnDomain = Migration
  { mgrTableName = tblName tableLogs
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableLogs
                     runQuery_ $ sqlCreateIndexSequentially tname $ indexOnColumn "component"
                     runQuery_ $ sqlAlterTable
                       tname
                       [ sqlAddColumn tblColumn { colName     = "domain"
                                                , colType     = ArrayT TextT
                                                , colNullable = False
                                                , colDefault  = Just "ARRAY[]::text[]"
                                                }
                       ]
                     runQuery_ $ sqlAlterTable tname [sqlAlterColumn "domain" "DROP DEFAULT"]
  }

createLogsTable :: MonadDB m => Migration m
createLogsTable = Migration
  { mgrTableName = tblName tableLogs
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName    = "logs"
        , tblVersion = 1
        , tblColumns =
          [ tblColumn { colName     = "insertion_order"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName     = "insertion_time"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          , tblColumn { colName     = "time"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          , tblColumn { colName = "level", colType = TextT, colNullable = False }
          , tblColumn { colName = "component", colType = TextT, colNullable = False }
          , tblColumn { colName = "message", colType = TextT, colNullable = False }
          , tblColumn { colName = "data", colType = JsonbT, colNullable = False }
          ]
        , tblIndexes = [indexOnColumn "time"]
        }
  }
