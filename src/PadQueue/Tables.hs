module PadQueue.Tables (tablePadQueue) where

import DB

tablePadQueue :: Table
tablePadQueue = tblTable {
    tblName = "padqueue"
  , tblVersion = 4
  , tblCreateOrValidate = \desc -> case desc of
      [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("document_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       , ("signatorylink_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
       ] -> return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE padqueue ("
          <> "  user_id BIGINT NOT NULL"
          <> ", document_id BIGINT NOT NULL"
          <> ", signatorylink_id BIGINT NOT NULL"
          <> ", CONSTRAINT pk_padqueue PRIMARY KEY (user_id)"

          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "document_id" "documents" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     , (tblForeignKeyColumn "signatorylink_id" "signatory_links" "id")
                       { fkOnDelete = ForeignKeyCascade } ]
  }
