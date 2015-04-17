module Doc.API.Callback.Migrations where

import DB
import DB.Checks
import Doc.API.Callback.Tables

apiCallbacksAddIDColumn :: MonadDB m => Migration m
apiCallbacksAddIDColumn = Migration {
  mgrTable = tableDocumentApiCallbacks
, mgrFrom = 3
, mgrDo = do
  let tname = tblName tableDocumentApiCallbacks
      alterTable = sqlAlterTable tname
  runQuery_ $ alterTable ["RENAME COLUMN id TO document_id"]
  runQuery_ $ alterTable [
      "DROP CONSTRAINT pk__document_api_callbacks"
    , "DROP CONSTRAINT fk__document_api_callbacks__id__documents"
    , sqlAddColumn tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , "ADD CONSTRAINT pk__document_api_callbacks PRIMARY KEY (id)"
    , sqlAddFK tname $ (fkOnColumn "document_id" "documents" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  runQuery_ . sqlCreateIndex tname $ indexOnColumn "document_id"
}

addNameToCallbackConsumers :: MonadDB m => Migration m
addNameToCallbackConsumers = Migration {
  mgrTable = tableDocumentApiCallbackConsumers
, mgrFrom = 1
, mgrDo = do
  runSQL_ "ALTER TABLE document_api_callback_consumers ADD COLUMN name TEXT NOT NULL DEFAULT 'document_api_callbacks'"
  runSQL_ "ALTER TABLE document_api_callback_consumers ALTER COLUMN name DROP DEFAULT"
}

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
      sqlAddColumn tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , sqlAddColumn tblColumn { colName = "reserved_by", colType = BigIntT }
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
