module Doc.API.Callback.Migrations where

import DB
import DB.Checks
import Doc.API.Callback.Tables

updateApiCallbacksForNewConsumer :: MonadDB m => Migration m
updateApiCallbacksForNewConsumer = Migration {
  mgrTable = tableDocumentApiCallbacks
, mgrFrom = 2
, mgrDo = do
  let Table{..} = tableDocumentApiCallbacks
      alterTable = sqlAlterTable tblName
  runQuery_ $ alterTable ["RENAME COLUMN document_id TO id"]
  runQuery_ $ alterTable ["RENAME CONSTRAINT fk__document_api_callbacks__document_id__documents TO fk__document_api_callbacks__id__documents"]
  runQuery_ $ alterTable ["RENAME COLUMN expires TO run_at"]
  runQuery_ $ alterTable ["RENAME COLUMN attempt TO attempts"]
  runQuery_ $ alterTable [
      sqlAddColumn tblColumn { colName = "reserved_by", colType = BigIntT }
    , sqlAddFK tblName $ (fkOnColumn "reserved_by" "document_api_callback_consumers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
}

createTableDocumentApiCallbackConsumers :: MonadDB m => Migration m
createTableDocumentApiCallbackConsumers = Migration {
  mgrTable = tableDocumentApiCallbackConsumers
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "document_api_callback_consumers"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
    ]
    , tblPrimaryKey = pkOnColumn "id"
  }
}

removeDuplicateIndexFromDocumentApiCallbacks :: MonadDB m => Migration m
removeDuplicateIndexFromDocumentApiCallbacks = Migration {
  mgrTable = tableDocumentApiCallbacks
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableDocumentApiCallbacks
  runQuery_ . sqlDropIndex tblName $ indexOnColumn "document_id"
}
