module EID.Authentication.Migrations (
    addSignatoryIPToEIDAuthentications
  , addSMSPinAuthAdjustmentsToEIDAuthentications
  , addFIAuthChecksToEIDAuthentications
  , addAuthenticationKindToEIDAuthentications
  , addEmailToEIDAuthentications
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
          "check_cgi_eid_authentications_have_all_required_fields"
        , "check_nets_eid_authentications_have_all_required_fields"
        ]
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) $ map sqlAddValidCheck
        [ tblCheck
          { chkName = "check_cgi_se_bankid_authentications_have_all_required_fields"
          , chkCondition = "provider = 1 AND ocsp_response IS NOT NULL AND signatory_personal_number IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 1"
          }
        , tblCheck
          { chkName = "check_nets_se_bankid_authentications_have_all_required_fields"
          , chkCondition = "provider = 2 AND internal_provider IS NOT NULL AND signatory_date_of_birth IS NOT NULL AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 2"
          }
        , tblCheck
          { chkName = "check_nets_dk_nemid_authentications_have_all_required_fields"
          , chkCondition = "provider = 3 AND signature IS NOT NULL AND signatory_name IS NOT NULL OR provider <> 3"
          }
        , tblCheck
          { chkName = "check_sms_pin_authentications_have_all_required_fields"
          , chkCondition = "provider = 4 AND signatory_phone_number IS NOT NULL OR provider <> 4"
        }
        ]
  }

addFIAuthChecksToEIDAuthentications :: MonadDB m => Migration m
addFIAuthChecksToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 4
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications) $ map sqlAddValidCheck
        [ tblCheck
          { chkName = "check_nets_fi_tupas_authentications_have_all_required_fields"
          , chkCondition = "provider = 5 AND signatory_name IS NOT NULL AND signatory_date_of_birth IS NOT NULL OR provider <> 5"
          }
        ]
  }

addAuthenticationKindToEIDAuthentications :: MonadDB m => Migration m
addAuthenticationKindToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 5
  , mgrAction = StandardMigration $ do
      let tname = tblName tableEIDAuthentications
      runQuery_ . sqlDropIndex tname $ indexOnColumn "signatory_link_id"
      runQuery_ $ sqlAlterTable tname
        [ sqlDropPK tname
        , sqlAddColumn tblColumn { colName = "auth_kind", colType = SmallIntT, colNullable = False, colDefault = Just "1" }
        ]
      runQuery_ $ sqlAlterTable tname
        [ sqlAlterColumn "auth_kind" "DROP DEFAULT"
        , sqlAddPK tname . fromJust $ pkOnColumns ["signatory_link_id", "auth_kind"]
        ]
  }

addEmailToEIDAuthentications :: MonadDB m => Migration m
addEmailToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 6
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications)  [ sqlAddColumn $
          tblColumn { colName = "signatory_email", colType = TextT }
        ]
  }

