module Folder.Migrations (
    createTableFolders
  , addNotNullConstraintToFolderNameColumn
  ) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import Folder.Tables

createTableFolders :: MonadDB m => Migration m
createTableFolders = Migration
  { mgrTableName = tblName tableFolders
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
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
  }

addNotNullConstraintToFolderNameColumn :: (MonadDB m) => Migration m
addNotNullConstraintToFolderNameColumn = Migration
  { mgrTableName = tblName tableFolders
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runSQL_ "UPDATE folders SET name='' WHERE name IS NULL"
                     runSQL_ "ALTER TABLE folders ALTER COLUMN name SET NOT NULL"
  }
