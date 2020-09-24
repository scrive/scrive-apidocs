module EID.Signature.Tables where

import DB

tableEIDSignatures :: Table
tableEIDSignatures = tblTable
  { tblName        = "eid_signatures"
  , tblVersion     = 10
  , tblColumns     =
    [ tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
  -- text for now, can be changed to bytea later if necessary
    , tblColumn { colName = "data", colType = TextT }
    , tblColumn { colName = "signature", colType = BinaryT }
    , tblColumn { colName = "certificate", colType = BinaryT }
    , tblColumn { colName = "signatory_name", colType = TextT }
    , tblColumn { colName = "signatory_personal_number", colType = TextT }
    , tblColumn { colName = "ocsp_response", colType = BinaryT }
    , tblColumn { colName = "signatory_ip", colType = TextT }
    , tblColumn { colName = "signatory_date_of_birth", colType = TextT }
    , tblColumn { colName = "signatory_email", colType = TextT }
    , tblColumn { colName = "signatory_phone_number", colType = TextT }
    ]
-- only one signature per signatory. can be relaxed later if necessary.
  , tblPrimaryKey  = pkOnColumn "signatory_link_id"
  , tblChecks      =
    [ tblCheck
      { chkName      = "eid_signatures_certificate_well_defined"
      , chkCondition =
      -- certificate was used only with legacy bank_id/nordea/telia
        "provider <= 3 AND certificate IS NOT NULL OR provider > 3 AND certificate IS NULL"
      }
    , tblCheck
      { chkName      = "eid_signatures_signatory_name_well_defined"
      , chkCondition =
      -- signatory name wasn't saved with all legacy implementations
        "provider <= 4 AND signatory_name IS NULL OR provider > 4 AND signatory_name IS NOT NULL"
      }
    , tblCheck
      { chkName      = "eid_signatures_ocsp_response_well_defined"
      , chkCondition =
      -- ocsp_response is used with legacy mobile_bank_id and cgi_grp_bank_id
        "(provider <= 3 OR provider >= 6) AND ocsp_response IS NULL OR (provider = 4 OR provider = 5 OR provider = 12) AND ocsp_response IS NOT NULL"
      }
    , tblCheck
      { chkName      = "eid_signatures_signatory_personal_number_well_defined"
      , chkCondition =
        "(provider = ANY (ARRAY[1, 2, 3, 4, 13])) = (signatory_personal_number IS NULL)"
      }
    ]
  , tblForeignKeys =
    [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [indexOnColumn "signatory_link_id"]
  }
