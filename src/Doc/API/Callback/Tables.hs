module Doc.API.Callback.Tables where

import DB

tableDocumentApiCallbacks :: Table
tableDocumentApiCallbacks = tblTable {
    tblName = "document_api_callbacks"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "url", colType = TextT, colNullable = False }
    , tblColumn { colName = "attempt", colType = IntegerT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "document_id"
  , tblForeignKeys = [
      (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
