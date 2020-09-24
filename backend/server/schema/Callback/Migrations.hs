module Callback.Migrations where

import Database.PostgreSQL.PQTypes.Checks

import DB

createCallbackConsumersTable :: MonadDB m => Migration m
createCallbackConsumersTable = Migration
  { mgrTableName = "callback_consumers"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName       = "callback_consumers"
      , tblVersion    = 1
      , tblColumns    =
        [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
        , tblColumn { colName     = "last_activity"
                    , colType     = TimestampWithZoneT
                    , colNullable = False
                    }
        , tblColumn { colName = "name", colType = TextT, colNullable = False }
        ]
      , tblPrimaryKey = pkOnColumn "id"
      }
  }

createCallbacksTable :: MonadDB m => Migration m
createCallbacksTable = Migration
  { mgrTableName = "callbacks"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
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
  }
