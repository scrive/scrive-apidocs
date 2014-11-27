module EID.Signature.Tables where

import DB

tableEIDSignatures :: Table
tableEIDSignatures = tblTable {
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
-- only one signature per signatory. can be relaxed later if necessary.
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
