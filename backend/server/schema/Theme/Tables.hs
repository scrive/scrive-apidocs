module Theme.Tables where

import qualified Data.ByteString as BS

import DB
import KontraPrelude

tableThemes:: Table
tableThemes = tblTable {
    tblName = "themes"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False  }
    , tblColumn { colName = "logo", colType = BinaryT, colNullable = False  }
    , tblColumn { colName = "brand_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "brand_text_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "action_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "action_text_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "action_secondary_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "action_secondary_text_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "positive_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "positive_text_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "negative_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "negative_text_color", colType = CustomT "color", colNullable = False }
    , tblColumn { colName = "font", colType = CustomT "font", colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableThemeOwnership:: Table
tableThemeOwnership = tblTable {
    tblName = "theme_owners"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "theme_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT, colNullable = True  }
    , tblColumn { colName = "domain_id", colType = BigIntT, colNullable = True  }
    ]
  , tblPrimaryKey = pkOnColumn "theme_id"
  , tblChecks = [Check "check_theme_is_owned_by_company_or_domain" "(company_id IS NULL OR domain_id IS NULL) AND (company_id IS NOT NULL OR domain_id IS NOT NULL)"] -- XOR
  , tblForeignKeys = [
      (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade },
      (fkOnColumn "domain_id" "branded_domains" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

domainColor :: Domain
domainColor = Domain {
  domName = "color"
  , domType = TextT
  , domNullable = False
  , domDefault = Nothing
  , domChecks = mkChecks [Check "color_hex" "VALUE ~ '^#[0-9a-f]{6}$'::text"]
}

domainFont :: Domain
domainFont = Domain {
    domName = "font"
    , domType = TextT
    , domNullable = False
    , domDefault = Nothing
    , domChecks = mkChecks [Check "font_in_set" sql]
  } where sql = rawSQL (BS.intercalate " OR " $ map value fonts) ()
          value font = BS.concat ["VALUE = '", font, "'::text"]
          fonts :: [BS.ByteString] = ["\"arial black\",sans-serif"
            , "\"arial narrow\",sans-serif"
            , "\"comic sans ms\",sans-serif"
            , "\"courier new\",monospace"
            , "\"Source Sans Pro\", \"Helvetica Neue\", Arial, sans-serif"
            , "garamond,serif"
            , "georgia,serif"
            , "\"times new roman\",serif"
            , "tahoma,sans-serif"
            , "\"trebuchet ms\",sans-serif"
            , "verdana,sans-serif"
            , "arial,helvetica,sans-serif"
            , "helvetica,sans-serif"]
