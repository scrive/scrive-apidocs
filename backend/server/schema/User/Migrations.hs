module User.Migrations where

import DB
import User.Tables

addPasswordAlgorithmVersionColumn :: MonadDB m => Migration m
addPasswordAlgorithmVersionColumn = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 20
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUsers)  [ sqlAddColumn $
          tblColumn { colName = "password_algorithm", colType = SmallIntT, colNullable = True }
        ]
  }

addUserTOTPKeyColumn :: MonadDB m => Migration m
addUserTOTPKeyColumn = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 21
  , mgrAction = StandardMigration $
      runQuery_ $ sqlAlterTable (tblName tableUsers)
        [ sqlAddColumn $
            tblColumn { colName = "totp_key", colType = BinaryT, colNullable = True }
        , sqlAddColumn $
            tblColumn { colName = "totp_active", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
  }
