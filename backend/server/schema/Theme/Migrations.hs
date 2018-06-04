module Theme.Migrations where

import Control.Monad.Catch

import DB
import Theme.Tables

themeOwnershipAddUserGroupID :: (MonadThrow m, MonadDB m) => Migration m
themeOwnershipAddUserGroupID = Migration {
  mgrTableName = tblName tableThemeOwnership
, mgrFrom = 1
, mgrAction = StandardMigration $ runQuery_ $ do
    sqlAlterTable (tblName tableThemeOwnership)  [
        sqlAddColumn $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
      , sqlAddFK (tblName tableThemeOwnership) $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
      ]
}
