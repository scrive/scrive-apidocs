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

usersAddUserGroupID :: MonadDB m => Migration m
usersAddUserGroupID = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 22
  , mgrAction = StandardMigration $ do
      let tname = tblName tableUsers
      runQuery_ . sqlAlterTable tname $
        [
          sqlAddColumn $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
        , sqlAddFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
        ]
      runQuery_ . sqlCreateIndex tname $ indexOnColumn "user_group_id"
  }

usersMakeUserGroupIDNotNull :: MonadDB m => Migration m
usersMakeUserGroupIDNotNull = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 23
  , mgrAction = StandardMigration $ do
      let tname = tblName tableUsers
      runQuery_ . sqlAlterTable tname $
        [ sqlAlterColumn "user_group_id" "SET NOT NULL"
        , sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
        , sqlAddFK tname $ fkOnColumn "user_group_id" "user_groups" "id"
        ]
  }
