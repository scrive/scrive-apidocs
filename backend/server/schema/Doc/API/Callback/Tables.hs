module Doc.API.Callback.Tables where

import DB

tableDocumentApiCallbackConsumers :: Table
tableDocumentApiCallbackConsumers = tblTable
  { tblName       = "document_api_callback_consumers"
  , tblVersion    = 2
  , tblColumns = [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                 , tblColumn { colName     = "last_activity"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableDocumentApiCallbacks :: Table
tableDocumentApiCallbacks = tblTable
  { tblName        = "document_api_callbacks"
  , tblVersion     = 5
  , tblColumns     =
    [ tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "url", colType = TextT, colNullable = False }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "api_version", colType = SmallIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [ (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "document_api_callback_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                                        }
    ]
  , tblIndexes     = [indexOnColumn "document_id"]
  }
