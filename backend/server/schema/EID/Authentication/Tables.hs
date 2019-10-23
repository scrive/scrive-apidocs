module EID.Authentication.Tables where

import DB

tableEIDAuthentications :: Table
tableEIDAuthentications = tblTable
  { tblName        = "eid_authentications"
  , tblVersion     = 8
  , tblColumns     =
    [ tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "signature", colType = BinaryT }
    , tblColumn { colName = "signatory_name", colType = TextT }
    , tblColumn { colName = "signatory_personal_number", colType = TextT }
    , tblColumn { colName = "ocsp_response", colType = BinaryT }
    , tblColumn { colName = "session_id", colType = BigIntT }
    , tblColumn { colName = "internal_provider", colType = SmallIntT }
    , tblColumn { colName = "signatory_phone_number", colType = TextT }
    , tblColumn { colName = "signatory_date_of_birth", colType = TextT }
    , tblColumn { colName = "signatory_ip", colType = TextT }
    , tblColumn { colName = "auth_kind", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "signatory_email", colType = TextT }
    , tblColumn { colName = "provider_customer_id", colType = TextT }
    ]
  , tblPrimaryKey  = pkOnColumns ["signatory_link_id", "auth_kind"]
  , tblChecks      =
    [ -- Minimal checks to guarantee that fetch error on fromJust will not happen
      tblCheck
      { chkName      = "check_cgi_se_bankid_authentications_have_all_required_fields"
      , chkCondition =
        "provider = 1 \
      \AND ocsp_response IS NOT NULL \
      \AND signatory_personal_number IS NOT NULL \
      \AND signature IS NOT NULL \
      \AND signatory_name IS NOT NULL \
      \OR provider <> 1"
      }
    , tblCheck
      { chkName      = "check_nets_se_bankid_authentications_have_all_required_fields"
      , chkCondition =
        "provider = 2 \
        \AND internal_provider IS NOT NULL \
        \AND signatory_date_of_birth IS NOT NULL \
        \AND signature IS NOT NULL \
        \AND signatory_name IS NOT NULL \
        \OR provider <> 2"
      }
    , tblCheck
      { chkName      = "check_nets_dk_nemid_authentications_have_all_required_fields"
      , chkCondition =
        "provider = 3 \
        \AND signature IS NOT NULL \
        \AND signatory_name IS NOT NULL \
        \OR provider <> 3"
      }
    , tblCheck
      { chkName      = "check_sms_pin_authentications_have_all_required_fields"
      , chkCondition =
        "provider = 4 \
        \AND signatory_phone_number IS NOT NULL \
        \OR provider <> 4"
      }
    , tblCheck
      { chkName      = "check_nets_fi_tupas_authentications_have_all_required_fields"
      , chkCondition =
        "provider = 5 \
        \AND signatory_name IS NOT NULL \
        \AND signatory_date_of_birth IS NOT NULL \
        \OR provider <> 5"
      }
    ]
  , tblForeignKeys =
    [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    , (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeySetNull }
    ]
  }
