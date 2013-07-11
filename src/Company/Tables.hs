module Company.Tables where

import DB

tableCompanies :: Table
tableCompanies = tblTable {
    tblName = "companies"
  , tblVersion = 13
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "number", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "address", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "zip", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "city", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "country", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "ip_address_mask_list", colType = TextT }
    , tblColumn { colName = "email_bordercolour", colType = TextT }
    , tblColumn { colName = "email_font", colType = TextT }
    , tblColumn { colName = "email_buttoncolour", colType = TextT }
    , tblColumn { colName = "email_emailbackgroundcolour", colType = TextT }
    , tblColumn { colName = "email_backgroundcolour", colType = TextT }
    , tblColumn { colName = "email_textcolour", colType = TextT }
    , tblColumn { colName = "email_logo", colType = BinaryT }
    , tblColumn { colName = "signview_logo", colType = BinaryT }
    , tblColumn { colName = "signview_textcolour", colType = TextT }
    , tblColumn { colName = "signview_textfont", colType = TextT }
    , tblColumn { colName = "signview_barscolour", colType = TextT }
    , tblColumn { colName = "signview_barstextcolour", colType = TextT }
    , tblColumn { colName = "signview_backgroundcolour", colType = TextT }
    , tblColumn { colName = "custom_logo", colType = BinaryT }
    , tblColumn { colName = "custom_barscolour", colType = TextT }
    , tblColumn { colName = "custom_barstextcolour", colType = TextT }
    , tblColumn { colName = "custom_barssecondarycolour", colType = TextT }
    , tblColumn { colName = "custom_backgroundcolour", colType = TextT }
    ]
  , tblPrimaryKey = ["id"]
  }
