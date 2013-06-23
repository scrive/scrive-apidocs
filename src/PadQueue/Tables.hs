module PadQueue.Tables (tablePadQueue) where

import DB

tablePadQueue :: Table
tablePadQueue = tblTable {
    tblName = "padqueue"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signatorylink_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["user_id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (tblForeignKeyColumn "signatorylink_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
