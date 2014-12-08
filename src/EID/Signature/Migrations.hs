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
    , tblColumn { colName = "data", colType = TextT, colNullable = False }
    , tblColumn { colName = "signature", colType = BinaryT, colNullable = False }
    , tblColumn { colName = "certificate", colType = BinaryT }
    , tblColumn { colName = "signatory_name", colType = TextT }
    , tblColumn { colName = "ocsp_response", colType = BinaryT }
    ]
  , tblChecks = [
      TableCheck "eid_signatures_certificate_well_defined"
        -- certificate was used only with legacy bank_id/nordea/telia
        "provider <= 3 AND certificate IS NOT NULL OR provider > 3 AND certificate IS NULL"
    , TableCheck "eid_signatures_signatory_name_well_defined"
        -- signatory name wasn't saved with all legacy implementations
        "provider <= 4 AND signatory_name IS NULL OR provider > 4 AND signatory_name IS NOT NULL"
    , TableCheck "eid_signatures_signatory_personal_number_well_defined"
        -- signatory personal number wasn't saved with all legacy implementations
        "provider <= 4 AND signatory_personal_number IS NULL OR provider > 4 AND signatory_personal_number IS NOT NULL"
    , TableCheck "eid_signatures_ocsp_response_well_defined"
        -- ocsp_response is used with legacy mobile_bank_id and cgi_grp_bank_id
        "provider <= 3 AND ocsp_response IS NULL OR provider > 3 AND ocsp_response IS NOT NULL"
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
