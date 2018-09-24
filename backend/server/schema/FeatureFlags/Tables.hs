module FeatureFlags.Tables (
  tableFeatureFlags
) where

import DB

tableFeatureFlags :: Table
tableFeatureFlags = tblTable {
    tblName = "feature_flags"
  , tblVersion = 9
  , tblColumns = [
      tblColumn { colName = "can_use_templates", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_branding", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_author_attachments", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_signatory_attachments", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_mass_sendout", colType = BoolT, colNullable = False, colDefault = Just "true" }

    , tblColumn { colName = "can_use_sms_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_sms_confirmations", colType = BoolT, colNullable = False, colDefault = Just "true" }

    , tblColumn { colName = "can_use_dk_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_no_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_se_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_se_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_sms_pin_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_no_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_sms_pin_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_dk_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "can_use_standard_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_standard_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "flags_for_admin", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_email_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_api_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_pad_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "can_use_fi_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
    ]
  , tblPrimaryKey = pkOnColumns ["user_group_id", "flags_for_admin"]
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
