module Theme.Tables where

import qualified Data.Text as T

import DB

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

tableThemeOwnership :: Table
tableThemeOwnership = tblTable {
    tblName = "theme_owners"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "theme_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "domain_id", colType = BigIntT, colNullable = True  }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True  }
    ]
  , tblPrimaryKey = pkOnColumn "theme_id"
  , tblChecks =
    [ tblCheck
      { chkName = "check_theme_is_owned_by_user_group_or_domain"
      , chkCondition =
            "(user_group_id IS \  \NULL OR domain_id IS \  \NULL) \
        \AND (user_group_id IS NOT NULL OR domain_id IS NOT NULL)"
      }
    ]
  , tblForeignKeys = [
      (fkOnColumn "domain_id" "branded_domains" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

domainColor :: Domain
domainColor = Domain {
  domName = "color"
  , domType = TextT
  , domNullable = False
  , domDefault = Nothing
  , domChecks = mkChecks
    [ tblCheck
      { chkName = "color_hex"
      , chkCondition = "VALUE ~ '^#[0-9a-f]{6}$'::text"
      }
    ]
}

domainFont :: Domain
domainFont = Domain {
    domName = "font"
    , domType = TextT
    , domNullable = False
    , domDefault = Nothing
    , domChecks = mkChecks
      [ tblCheck
        { chkName = "font_in_set"
        , chkCondition = sql
        }]
  } where sql = rawSQL (T.intercalate " OR " $ map value fonts) ()
          value font = T.concat ["VALUE = '", font, "'::text"]
          fonts :: [Text] = ["\"arial black\",sans-serif"
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
