module DB.Versions where

import DB.Model
import KontraPrelude

tableVersions :: Table
tableVersions = tblTable {
    tblName = "table_versions"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "version", colType = IntegerT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "name"
  }
