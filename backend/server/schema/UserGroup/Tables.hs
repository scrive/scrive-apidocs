module UserGroup.Tables where

import DB

tableUserGroups :: Table
tableUserGroups = tblTable {
    tblName = "user_groups"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "parent_group_id", colType = BigIntT, colNullable = True }
    , tblColumn { colName = "parent_group_path", colType = ArrayT BigIntT, colDefault = Just "ARRAY[]::bigint[]" }
    , tblColumn { colName = "name", colType = TextT }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      -- do not allow to delete groups which still contains some other groups
      -- always must delete the child groups explicitely
      (fkOnColumn "parent_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyRestrict }
    ]
  }

ctUserGroup :: CompositeType
ctUserGroup = CompositeType {
    ctName = "user_group"
  , ctColumns = [
      CompositeColumn { ccName = "id", ccType = BigIntT }
    , CompositeColumn { ccName = "name", ccType = TextT }
    , CompositeColumn { ccName = "parent_group_id", ccType = BigIntT }
    , CompositeColumn { ccName = "invoicing", ccType = CustomT "user_group_invoicing" }
    , CompositeColumn { ccName = "user_group_setting", ccType = CustomT "user_group_setting" }
    , CompositeColumn { ccName = "user_group_addresses", ccType = CustomT "user_group_address" }
    , CompositeColumn { ccName = "ui", ccType = CustomT "user_group_ui" }
    ]
  }

tableUserGroupSettings :: Table
tableUserGroupSettings = tblTable {
    tblName = "user_group_settings"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "ip_address_mask_list", colType = TextT }
    , tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }
    , tblColumn { colName = "cgi_display_name", colType = TextT }
    , tblColumn { colName = "sms_provider", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
    , tblColumn { colName = "cgi_service_id", colType = TextT }
    , tblColumn { colName = "pad_app_mode", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
    , tblColumn { colName = "pad_earchive_enabled", colType = BoolT, colNullable = False, colDefault = Just "true" }
    ]
  , tblPrimaryKey = pkOnColumn "user_group_id"
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

-- the CT is named user_group_setting instead of naming the table user_group_settingss
ctUserGroupSettings :: CompositeType
ctUserGroupSettings = CompositeType {
    ctName = "user_group_setting"
  , ctColumns = [
      CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
    , CompositeColumn { ccName = "idle_doc_timeout", ccType = SmallIntT }
    , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
    , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
    , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
    , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
    , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
    ]
  }

tableUserGroupAddresses :: Table
tableUserGroupAddresses = tblTable {
    tblName = "user_group_addresses"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "company_number", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "address", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "zip", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "city", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "country", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    ]
  , tblPrimaryKey = pkOnColumn "user_group_id"
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

ctUserGroupAddress :: CompositeType
ctUserGroupAddress = CompositeType {
    ctName = "user_group_address"
  , ctColumns = [
      CompositeColumn { ccName = "company_number", ccType = TextT }
    , CompositeColumn { ccName = "address", ccType = TextT }
    , CompositeColumn { ccName = "zip", ccType = TextT }
    , CompositeColumn { ccName = "city", ccType = TextT }
    , CompositeColumn { ccName = "country", ccType = TextT }
    ]
  }

tableUserGroupUIs :: Table
tableUserGroupUIs = tblTable {
    tblName = "user_group_uis"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_group_id",  colType = BigIntT, colNullable = False }
    , tblColumn { colName = "mail_theme",     colType = BigIntT}
    , tblColumn { colName = "signview_theme", colType = BigIntT}
    , tblColumn { colName = "service_theme",  colType = BigIntT}
    , tblColumn { colName = "browser_title",  colType = TextT}
    , tblColumn { colName = "sms_originator", colType = TextT}
    , tblColumn { colName = "favicon",        colType = BinaryT}
    ]
  , tblPrimaryKey = pkOnColumn "user_group_id"
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade },
      (fkOnColumn "mail_theme" "themes" "id"),
      (fkOnColumn "signview_theme" "themes" "id"),
      (fkOnColumn "service_theme" "themes" "id")
    ]
  }

ctUserGroupUI :: CompositeType
ctUserGroupUI = CompositeType {
    ctName = "user_group_ui"
  , ctColumns = [
      CompositeColumn { ccName = "mail_theme", ccType = BigIntT }
    , CompositeColumn { ccName = "signview_theme", ccType = BigIntT }
    , CompositeColumn { ccName = "service_theme", ccType = BigIntT }
    , CompositeColumn { ccName = "browser_title", ccType = TextT }
    , CompositeColumn { ccName = "sms_originator", ccType = TextT }
    , CompositeColumn { ccName = "favicon", ccType = BinaryT }
    ]
  }

tableUserGroupInvoicings :: Table
tableUserGroupInvoicings = tblTable {
    tblName = "user_group_invoicings"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "user_group_id",  colType = BigIntT, colNullable = False }
    , tblColumn { colName = "invoicing_type", colType = SmallIntT, colNullable = False}
    , tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = True}
    ]
  , tblPrimaryKey = pkOnColumn "user_group_id"
  , tblForeignKeys = [
      (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblChecks = [
      Check "user_group_invoicing_type_matches_payplan" $
           "invoicing_type = 1 AND payment_plan IS NULL \
        \OR invoicing_type = 2 \
        \OR invoicing_type = 3 AND payment_plan IS NOT NULL"
    ]
  }

ctUserGroupInvoicing :: CompositeType
ctUserGroupInvoicing = CompositeType {
    ctName = "user_group_invoicing"
  , ctColumns = [
      CompositeColumn { ccName = "invoicing_type", ccType = SmallIntT }
    , CompositeColumn { ccName = "payment_plan", ccType = SmallIntT }
    ]
  }
