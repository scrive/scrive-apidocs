module FeatureFlags.Tables (
  tableFeatureFlags
, ctFeatureFlags
) where

import DB

tableFeatureFlags :: Table
tableFeatureFlags = tblTable {
    tblName = "feature_flags"
  , tblVersion = 19
  , tblColumns = [
      tblColumn { colName = "can_use_templates", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_branding", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_author_attachments", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_signatory_attachments", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_mass_sendout", colType = BoolT, colNullable = False }

    , tblColumn { colName = "can_use_sms_invitations", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_sms_confirmations", colType = BoolT, colNullable = False }

    , tblColumn { colName = "can_use_dk_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_no_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_se_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_se_authentication_to_sign", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_sms_pin_authentication_to_sign", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_no_authentication_to_sign", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_sms_pin_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_dk_authentication_to_sign", colType = BoolT, colNullable = False }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "can_use_standard_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_standard_authentication_to_sign", colType = BoolT, colNullable = False }
    , tblColumn { colName = "flags_for_admin", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_email_invitations", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_api_invitations", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_pad_invitations", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_fi_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_email_confirmations", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_shareable_links", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_forwarding", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_document_party_notifications", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_verimi_authentication_to_view", colType = BoolT, colNullable = False }
    , tblColumn { colName = "can_use_idin_authentication_to_view", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumns ["user_group_id", "flags_for_admin"]
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

ctFeatureFlags :: CompositeType
ctFeatureFlags = CompositeType {
    ctName = "feature_flags_c3"
  , ctColumns =
    [ CompositeColumn { ccName = "can_use_templates", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_branding", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_author_attachments", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_signatory_attachments", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_mass_sendout", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_sms_invitations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_sms_confirmations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_dk_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_dk_authentication_to_sign", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_fi_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_no_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_no_authentication_to_sign", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_se_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_se_authentication_to_sign", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_sms_pin_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_sms_pin_authentication_to_sign", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_standard_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_standard_authentication_to_sign", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_verimi_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_idin_authentication_to_view", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_email_invitations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_email_confirmations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_api_invitations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_pad_invitations", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_shareable_links", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_forwarding", ccType = BoolT }
    , CompositeColumn { ccName = "can_use_document_party_notifications", ccType = BoolT }
    ]
  }
