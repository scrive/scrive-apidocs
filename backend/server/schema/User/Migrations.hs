module User.Migrations where

import qualified Data.ByteString as BS

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

usersDropCompanyID :: MonadDB m => Migration m
usersDropCompanyID = Migration {
    mgrTableName = tblName tableUsers
  , mgrFrom = 24
  , mgrAction = StandardMigration $ do
      let tname = tblName tableUsers
      runQuery_ $ sqlDropIndex tname $ (indexOnColumn "company_id")
      runQuery_ . sqlAlterTable tname $
        [ sqlDropFK tname $ (fkOnColumn "company_id" "companies" "id")
        , sqlDropColumn "company_id"

dropNotNullConstraintsWhenUserDeleted :: MonadDB m => Migration m
dropNotNullConstraintsWhenUserDeleted = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom = 25
  , mgrAction = StandardMigration $ do
      let sqlWhereIsDeletedUserID name =
            sqlWhereInSql name $ toSQLCommand $ sqlSelect "users" $ do
              sqlResult "id"
              sqlWhereIsNotNULL "deleted"

      runQuery_ $ sqlDelete "attachments" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "email_change_requests" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "oauth_access_token" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "oauth_api_token" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "oauth_temp_credential" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "sessions" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "sessions" $
        sqlWhereIsDeletedUserID "pad_user_id"
      runQuery_ $ sqlDelete "user_account_requests" $
        sqlWhereIsDeletedUserID "user_id"
      runQuery_ $ sqlDelete "user_callback_scheme" $
        sqlWhereIsDeletedUserID "user_id"

      runQuery_ $ sqlUpdate "users_history" $ do
        sqlSet "event_data" (Nothing :: Maybe String)
        sqlWhereIsDeletedUserID "user_id"

      runQuery_ $ sqlUpdate "users" $ do
        sqlSet "password"         ("" :: BS.ByteString)
        sqlSet "salt"             ("" :: BS.ByteString)
        sqlSet "first_name"       ("" :: String)
        sqlSet "last_name"        ("" :: String)
        sqlSet "personal_number"  ("" :: String)
        sqlSet "company_position" ("" :: String)
        sqlSet "phone"            ("" :: String)
        sqlSet "email"            ("" :: String)
        sqlSet "lang"             (1 :: Int)
        sqlWhereIsNotNULL "deleted"
  }
