module Company.Tables where

import DB
import KontraPrelude

tableCompanies :: Table
tableCompanies = tblTable {
    tblName = "companies"
  , tblVersion = 18
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "number", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "address", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "zip", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "city", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "country", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "ip_address_mask_list", colType = TextT }
    , tblColumn { colName = "allow_save_safety_copy", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }
    , tblColumn { colName = "cgi_display_name", colType = TextT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableCompanyUIs :: Table
tableCompanyUIs = tblTable {
    tblName = "company_uis"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "company_id",                  colType = BigIntT, colNullable = False }
    , tblColumn { colName = "mail_theme",                  colType = BigIntT}
    , tblColumn { colName = "signview_theme",              colType = BigIntT}
    , tblColumn { colName = "service_theme",               colType = BigIntT}
    , tblColumn { colName = "browser_title",               colType = TextT}
    , tblColumn { colName = "sms_originator",              colType = TextT}
    , tblColumn { colName = "favicon",                     colType = BinaryT}

    ]
  , tblPrimaryKey = pkOnColumn "company_id"
  , tblForeignKeys = [
      (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade },
      (fkOnColumn "mail_theme" "themes" "id"),
      (fkOnColumn "signview_theme" "themes" "id"),
      (fkOnColumn "service_theme" "themes" "id")
    ]
  }
