module Doc.API.Callback.Tables where

import DB

tableDocumentApiCallbacks :: Table
tableDocumentApiCallbacks = tblTable {
    tblName = "document_api_callbacks"
  , tblVersion = 1
  , tblCreateOrValidate = \desc -> case desc of
    [  ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     , ("expires", SqlColDesc {colType = SqlTimestampWithZoneT, colNullable = Just False})
     , ("url", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
     , ("attempt", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
     ] -> return TVRvalid
    [] -> do
      kRunRaw $ "CREATE TABLE document_api_callbacks ("
        <> "  document_id BIGINT NOT NULL"
        <> ", expires TIMESTAMPTZ NOT NULL"
        <> ", url TEXT NOT NULL"
        <> ", attempt INTEGER NOT NULL"
        <> ", CONSTRAINT pk_document_api_callbacks PRIMARY KEY (document_id)"
        <> ")"
      return TVRcreated
    _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
}
