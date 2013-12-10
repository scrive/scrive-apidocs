module Doc.API.Callback.Migrations where

import DB
import Doc.API.Callback.Tables

removeDuplicateIndexFromDocumentApiCallbacks :: MonadDB m => Migration m
removeDuplicateIndexFromDocumentApiCallbacks = Migration {
  mgrTable = tableDocumentApiCallbacks
, mgrFrom = 1
, mgrDo = do
  let Table{..} = tableDocumentApiCallbacks
  runQuery_ . sqlDropIndex tblName $ indexOnColumn "document_id"
}
