module User.APILog.Migrations
    (
      createAPILogsTable
    , createAPILogsTablePK
    )
    where

import Database.PostgreSQL.PQTypes.Checks

import DB
import User.APILog.Tables

createAPILogsTablePK :: MonadDB m => Migration m
createAPILogsTablePK = Migration
  { mgrTableName = tblName tableAPILogs
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableAPILogs)
                       [sqlAddPK (tblName tableAPILogs) (fromJust . pkOnColumn $ "id")]
  }

createAPILogsTable :: MonadDB m => Migration m
createAPILogsTable = Migration
  { mgrTableName = tblName tableAPILogs
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName    = "api_call_logs"
        , tblVersion = 1
        , tblColumns =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName     = "time"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "request_uri", colType = TextT, colNullable = False }
          , tblColumn { colName = "request_method", colType = TextT, colNullable = False }
          , tblColumn { colName     = "request_params_get"
                      , colType     = JsonT
                      , colNullable = False
                      }
          , tblColumn { colName     = "request_params_post"
                      , colType     = JsonT
                      , colNullable = False
                      }
          , tblColumn { colName     = "response_code"
                      , colType     = IntegerT
                      , colNullable = False
                      }
          , tblColumn { colName = "response_body", colType = TextT, colNullable = False }
          ]
        , tblIndexes = [indexOnColumn "user_id"]
        }
  }
