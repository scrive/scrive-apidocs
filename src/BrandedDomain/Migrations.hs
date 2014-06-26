module BrandedDomain.Migrations where

import DB
import DB.Checks
import BrandedDomain.Tables

createBrandedDomainsTable :: MonadDB m => Migration m
createBrandedDomainsTable =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 0
    , mgrDo = do
        createTable $ tblTable {
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
    , tblColumn { colName = "contact_email",                 colType = BinaryT, colNullable = True }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblIndexes = [
      indexOnColumn "url"
    ]
  }

}


addLogoImageDataToBrandedDomain :: MonadDB m => Migration m
addLogoImageDataToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 1
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains DROP COLUMN logolink"
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN logo BYTEA NULL"
    }

addNoReplyEmailToBrandedDomain :: MonadDB m => Migration m
addNoReplyEmailToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 2
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN noreply_email TEXT NOT NULL DEFAULT ''"
    }

addNoReplyEmailToBrandedDomainSetDefault :: MonadDB m => Migration m
addNoReplyEmailToBrandedDomainSetDefault =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 3
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ALTER COLUMN noreply_email SET DEFAULT ''"
    }

addMailsBorderColorToBrandedDomain :: MonadDB m => Migration m
addMailsBorderColorToBrandedDomain =
  Migration {
      mgrTable = tableBrandedDomains
    , mgrFrom = 4
    , mgrDo = do
        runSQL_ "ALTER TABLE branded_domains ADD COLUMN mails_border_color TEXT NOT NULL DEFAULT ''"
    }
