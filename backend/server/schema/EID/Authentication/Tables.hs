module EID.Authentication.Tables where

import DB

tableEIDAuthentications :: Table
tableEIDAuthentications = tblTable {
  tblName = "eid_authentications"
, tblVersion = 4
, tblColumns = [
    tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
  , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
  , tblColumn { colName = "signature", colType = BinaryT}
  , tblColumn { colName = "signatory_name", colType = TextT }
  , tblColumn { colName = "signatory_personal_number", colType = TextT}
  , tblColumn { colName = "ocsp_response", colType = BinaryT }
  , tblColumn { colName = "session_id", colType = BigIntT }
  , tblColumn { colName = "internal_provider", colType = SmallIntT }
  , tblColumn { colName = "signatory_phone_number", colType = TextT }
  , tblColumn { colName = "signatory_date_of_birth", colType = TextT}
  , tblColumn { colName = "signatory_ip", colType = TextT }
  ]
-- only one authentication per signatory. can be relaxed later if necessary.
, tblPrimaryKey = pkOnColumn "signatory_link_id"
, tblChecks = [
      -- Minimal checks to guarantee that fetch error on fromJust will not happen
      Check "check_cgi_se_bankid_authentications_have_all_required_fields"
          "provider = 1 AND ocsp_response IS NOT NULL AND signatory_personal_number IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 1"
    , Check "check_nets_se_bankid_authentications_have_all_required_fields"
          "provider = 2 AND internal_provider IS NOT NULL AND signatory_date_of_birth IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 2"
    , Check "check_nets_dk_nemid_authentications_have_all_required_fields"
          "provider = 3 AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 3"
    , Check "check_sms_pin_authentications_have_all_required_fields"
          "provider = 4 AND signatory_phone_number IS NOT NULL OR provider <> 4"
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
