module EID.Authentication.Tables where

import DB
import KontraPrelude

tableEIDAuthentications :: Table
tableEIDAuthentications = tblTable {
  tblName = "eid_authentications"
, tblVersion = 1
, tblColumns = [
    tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
  , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
  , tblColumn { colName = "signature", colType = BinaryT, colNullable = False }
  , tblColumn { colName = "signatory_name", colType = TextT, colNullable = False }
  , tblColumn { colName = "signatory_personal_number", colType = TextT, colNullable = False }
  , tblColumn { colName = "ocsp_response", colType = BinaryT, colNullable = False }
  , tblColumn { colName = "session_id", colType = BigIntT }
  ]
-- only one authentication per signatory. can be relaxed later if necessary.
, tblPrimaryKey = pkOnColumn "signatory_link_id"
, tblChecks = [
  ]
, tblForeignKeys = [
    (fkOnColumn "signatory_link_id" "signatory_links" "id") {
      fkOnDelete = ForeignKeyCascade
    },
    (fkOnColumn "session_id" "sessions" "id") {
      fkOnDelete = ForeignKeySetNull
    }
  ]
, tblIndexes = [
    indexOnColumn "signatory_link_id"
  ]
}
