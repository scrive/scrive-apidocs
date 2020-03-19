module Folder.Tables where

import DB

tableFolders :: Table
tableFolders = tblTable
  { tblName        = "folders"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT }
    , tblColumn { colName = "parent_id", colType = BigIntT, colNullable = True }
    , tblColumn { colName    = "parent_path"
                , colType    = ArrayT BigIntT
                , colDefault = Just "ARRAY[]::bigint[]"
                }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumnWithMethod "parent_path" GIN]
  , tblForeignKeys =
    [
      -- must delete child groups explicitly
     (fkOnColumn "parent_id" "folders" "id") { fkOnDelete = ForeignKeyRestrict }]
  }
