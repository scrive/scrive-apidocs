module EID.Signature.Migrations where

import DB
import DB.Checks

import EID.Signature.Tables

createEIDSignaturesTable :: MonadDB m => Migration m
createEIDSignaturesTable = Migration {
  mgrTable = tableEIDSignatures
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "eid_signatures"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "data", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "signature", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "signatory_name", colType = TextT }
    , tblColumn { colName = "certificate", colType = BinaryT }
    , tblColumn { colName = "ocsp_response", colType = BinaryT }
    ]
  , tblPrimaryKey = pkOnColumn "signatory_link_id"
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [
      indexOnColumn "signatory_link_id"
    ]
  }
}
