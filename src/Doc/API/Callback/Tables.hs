module Doc.API.Callback.Tables where

import DB

tableDocumentApiCallbackConsumers :: Table
tableDocumentApiCallbackConsumers = tblTable {
  tblName = "document_api_callback_consumers"
, tblVersion = 1
, tblColumns = [
    tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
  , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
  ]
  , tblPrimaryKey = pkOnColumn "id"
}

tableDocumentApiCallbacks :: Table
tableDocumentApiCallbacks = tblTable {
    tblName = "document_api_callbacks"
  , tblVersion = 3
  , tblColumns = [
      tblColumn { colName = "id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "url", colType = TextT, colNullable = False }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "reserved_by" "document_api_callback_consumers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  }
