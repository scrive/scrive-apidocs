module Company.Tables where

import DB

tableCompanies :: Table
tableCompanies = tblTable {
    tblName = "companies"
  , tblVersion = 25
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "number", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "address", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "zip", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "city", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "country", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "ip_address_mask_list", colType = TextT }
    , tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }
    , tblColumn { colName = "cgi_display_name", colType = TextT }
    , tblColumn { colName = "sms_provider", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
    , tblColumn { colName = "cgi_service_id", colType = TextT }
    , tblColumn { colName = "partner_id", colType = BigIntT, colNullable = False}
    , tblColumn { colName = "pad_app_mode", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
    , tblColumn { colName = "pad_earchive_enabled", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = False, colDefault = Just "0"}
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True}
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "partner_id" "partners" "id") { fkOnDelete = ForeignKeySetNull }
    , (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
    ]
  , tblIndexes = [
      indexOnColumn "user_group_id"
    ]
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
