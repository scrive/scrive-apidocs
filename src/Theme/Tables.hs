module Theme.Tables where

import DB

tableThemes:: Table
tableThemes = tblTable {
    tblName = "themes"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False  }
    , tblColumn { colName = "logo", colType = BinaryT, colNullable = False  }
    , tblColumn { colName = "brand_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "brand_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "action_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "action_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "action_secondary_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "action_secondary_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "positive_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "positive_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "negative_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "negative_text_color", colType = TextT, colNullable = False }
    , tblColumn { colName = "font", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableThemeOwnership:: Table
tableThemeOwnership = tblTable {
    tblName = "theme_owners"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "theme_id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "company_id", colType = BigIntT, colNullable = True  }
    , tblColumn { colName = "domain_id", colType = BigIntT, colNullable = True  }
    ]
  , tblPrimaryKey = pkOnColumn "theme_id"
  , tblChecks = [TableCheck "check_theme_is_owned_by_company_or_domain" "(company_id IS NULL OR domain_id IS NULL) AND (company_id IS NOT NULL OR domain_id IS NOT NULL)"] -- XOR
  , tblForeignKeys = [
      (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade },
      (fkOnColumn "domain_id" "branded_domains" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }
