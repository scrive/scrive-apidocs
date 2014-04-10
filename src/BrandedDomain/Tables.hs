module BrandedDomain.Tables (tableBrandedDomains) where

import DB

tableBrandedDomains :: Table
tableBrandedDomains = tblTable {
  tblName = "branded_domains"
  , tblVersion = 1
  , tblColumns =
    [ tblColumn { colName = "id",                            colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "url",                           colType = TextT, colNullable = False }
    , tblColumn { colName = "logolink",                      colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_color",                    colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_text_color",               colType = TextT, colNullable = False }
    , tblColumn { colName = "bars_secondary_color",          colType = TextT, colNullable = False }
    , tblColumn { colName = "background_color",              colType = TextT, colNullable = False }
    , tblColumn { colName = "background_color_external",     colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_background_color",        colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_button_color",            colType = TextT, colNullable = False }
    , tblColumn { colName = "mails_text_color",              colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_primary_color",        colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_primary_text_color",   colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_secondary_color",      colType = TextT, colNullable = False }
    , tblColumn { colName = "signview_secondary_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "button_class",                  colType = TextT, colNullable = False }
    , tblColumn { colName = "service_link_color",            colType = TextT, colNullable = False }
    , tblColumn { colName = "external_text_color",           colType = TextT, colNullable = False }
    , tblColumn { colName = "header_color",                  colType = TextT, colNullable = False }
    , tblColumn { colName = "text_color",                    colType = TextT, colNullable = False }
    , tblColumn { colName = "price_color",                   colType = TextT, colNullable = False }
    , tblColumn { colName = "sms_originator",                colType = TextT, colNullable = False }
    , tblColumn { colName = "email_originator",              colType = TextT, colNullable = False }
    , tblColumn { colName = "contact_email",                 colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes = [
      indexOnColumn "url"
    ]
  }
