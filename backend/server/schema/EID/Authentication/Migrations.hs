module EID.Authentication.Migrations (
    addSignatoryIPToEIDAuthentications
  , addSMSPinAuthAdjustmentsToEIDAuthentications
) where

import DB
import EID.Authentication.Tables

addSignatoryIPToEIDAuthentications :: MonadDB m => Migration m
addSignatoryIPToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications)  [ sqlAddColumn $
          tblColumn { colName = "signatory_ip", colType = TextT, colNullable = True }
        ]
  }

addSMSPinAuthAdjustmentsToEIDAuthentications :: MonadDB m => Migration m
addSMSPinAuthAdjustmentsToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 3
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) [
            sqlAlterColumn "signature" "DROP NOT NULL"
        ]
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) [
            sqlAlterColumn "signatory_name" "DROP NOT NULL"
        ]
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) $ map sqlDropCheck  [
          Check "check_cgi_eid_authentications_have_all_required_fields" ""
        , Check "check_nets_eid_authentications_have_all_required_fields" ""
        ]
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) $ map sqlAddCheck [
            Check "check_cgi_se_bankid_authentications_have_all_required_fields"
                "provider = 1 AND ocsp_response IS NOT NULL AND signatory_personal_number IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 1"
          , Check "check_nets_se_bankid_authentications_have_all_required_fields"
                "provider = 2 AND internal_provider IS NOT NULL AND signatory_date_of_birth IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 2"
          , Check "check_nets_dk_nemid_authentications_have_all_required_fields"
                "provider = 3 AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 3"
          , Check "check_sms_pin_authentications_have_all_required_fields"
                "provider = 4 AND signatory_phone_number IS NOT NULL OR provider <> 4"
          ]
  }
