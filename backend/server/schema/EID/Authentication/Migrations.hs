module EID.Authentication.Migrations where

import DB
import DB.Checks
import EID.Authentication.Tables
import KontraPrelude

createEIDAuthenticationTable :: MonadDB m => Migration m
createEIDAuthenticationTable = Migration {
  mgrTable = tableEIDAuthentications
, mgrFrom = 0
, mgrDo = createTable True tblTable {
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
}


addNetsFieldsToEIDAuthentication :: MonadDB m => Migration m
addNetsFieldsToEIDAuthentication = Migration {
      mgrTable = tableEIDAuthentications
    , mgrFrom = 1
    , mgrDo = do
        runSQL_ "ALTER TABLE eid_authentications ADD COLUMN internal_provider SMALLINT NULL"
        runSQL_ "ALTER TABLE eid_authentications ADD COLUMN signatory_phone_number TEXT NULL"
        runSQL_ "ALTER TABLE eid_authentications ADD COLUMN signatory_date_of_birth TEXT NULL"
        runSQL_ "ALTER TABLE eid_authentications ALTER COLUMN ocsp_response DROP NOT NULL"
        runSQL_ "ALTER TABLE eid_authentications ALTER COLUMN signatory_personal_number DROP NOT NULL"
        runQuery_ $ sqlAlterTable "eid_authentications" [
            sqlAddCheck $ Check "check_cgi_eid_authentications_have_all_required_fields"
              "provider = 1 AND ocsp_response IS NOT NULL AND signatory_personal_number IS NOT NULL OR provider <> 1"
          , sqlAddCheck $ Check "check_nets_eid_authentications_have_all_required_fields"
            "provider = 2 AND internal_provider IS NOT NULL AND signatory_date_of_birth IS NOT NULL OR provider <> 2"
          ]
}


