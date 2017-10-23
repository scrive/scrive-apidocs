module User.Migrations where

import DB
import KontraPrelude
import User.Tables

addPasswordStrengthColumn :: MonadDB m => Migration m
addPasswordStrengthColumn = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 20
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUsers)  [ sqlAddColumn $
          tblColumn { colName = "password_strength", colType = SmallIntT, colNullable = True }
        ]
  }
