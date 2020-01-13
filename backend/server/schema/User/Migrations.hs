module User.Migrations where

import Database.PostgreSQL.PQTypes.Checks
import qualified Data.ByteString as BS

import DB
import User.Tables

addPasswordAlgorithmVersionColumn :: MonadDB m => Migration m
addPasswordAlgorithmVersionColumn = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 20
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableUsers)
                       [ sqlAddColumn $ tblColumn { colName     = "password_algorithm"
                                                  , colType     = SmallIntT
                                                  , colNullable = True
                                                  }
                       ]
  }

addUserTOTPKeyColumn :: MonadDB m => Migration m
addUserTOTPKeyColumn = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 21
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableUsers)
                     [ sqlAddColumn $ tblColumn { colName     = "totp_key"
                                                , colType     = BinaryT
                                                , colNullable = True
                                                }
                     , sqlAddColumn $ tblColumn { colName     = "totp_active"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                     ]
  }

usersAddUserGroupID :: MonadDB m => Migration m
usersAddUserGroupID = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 22
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableUsers
      runQuery_
        . sqlAlterTable tname
        $ [ sqlAddColumn $ tblColumn { colName     = "user_group_id"
                                     , colType     = BigIntT
                                     , colNullable = True
                                     }
          , sqlAddValidFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
            { fkOnDelete = ForeignKeySetNull
            }
          ]
      runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "user_group_id"
  }

usersMakeUserGroupIDNotNull :: MonadDB m => Migration m
usersMakeUserGroupIDNotNull = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 23
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableUsers
      runQuery_
        . sqlAlterTable tname
        $ [ sqlAlterColumn "user_group_id" "SET NOT NULL"
          , sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
            { fkOnDelete = ForeignKeySetNull
            }
          , sqlAddValidFK tname $ fkOnColumn "user_group_id" "user_groups" "id"
          ]
  }

usersDropCompanyID :: MonadDB m => Migration m
usersDropCompanyID = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 24
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableUsers
                     runQuery_ $ sqlDropIndex tname $ (indexOnColumn "company_id")
                     runQuery_
                       . sqlAlterTable tname
                       $ [ sqlDropFK tname $ (fkOnColumn "company_id" "companies" "id")
                         , sqlDropColumn "company_id"
                         ]
  }

actuallyDeletePreviouslyDeletedUser :: MonadDB m => Migration m
actuallyDeletePreviouslyDeletedUser = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 25
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlUpdate "users" $ do
        sqlWith "deleted_users" . sqlSelect "users" $ do
          sqlResult "id"
          sqlWhereIsNotNULL "deleted"

        let sqlWhereIsDeletedUserID name =
              sqlWhere $ name <> " IN (SELECT id FROM deleted_users)"

        sqlWith "deleted_email_change_requests"
          . sqlDelete "email_change_requests"
          $ sqlWhereIsDeletedUserID "user_id"
        sqlWith "deleted_oauth_access_tokens"
          . sqlDelete "oauth_access_token"
          $ sqlWhereIsDeletedUserID "user_id"
        sqlWith "deleted_oauth_api_tokens"
          . sqlDelete "oauth_api_token"
          $ sqlWhereIsDeletedUserID "user_id"
        sqlWith "deleted_oauth_temp_credentials"
          . sqlDelete "oauth_temp_credential"
          $ sqlWhereIsDeletedUserID "user_id"
        sqlWith "deleted_sessions" . sqlDelete "sessions" $ sqlWhereIsDeletedUserID
          "user_id"
        sqlWith "deleted_pad_sessions" . sqlDelete "sessions" $ sqlWhereIsDeletedUserID
          "pad_user_id"
        sqlWith "deleted_user_account_requests"
          . sqlDelete "user_account_requests"
          $ sqlWhereIsDeletedUserID "user_id"
        sqlWith "deleted_user_callback_schemes"
          . sqlDelete "user_callback_scheme"
          $ sqlWhereIsDeletedUserID "user_id"

        sqlWith "deleted_users_histories" . sqlUpdate "users_history" $ do
          sqlSet "event_data" (Nothing :: Maybe String)
          sqlSet "ip"         (0 :: Int)
          sqlWhereIsDeletedUserID "user_id"

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

usersAddDataRetentionPolicy :: MonadDB m => Migration m
usersAddDataRetentionPolicy = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 26
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "users"
        [ sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_preparation", colType = SmallIntT }
        , sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_closed", colType = SmallIntT }
        , sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_canceled", colType = SmallIntT }
        , sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_timedout", colType = SmallIntT }
        , sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_rejected", colType = SmallIntT }
        , sqlAddColumn
          $ tblColumn { colName = "idle_doc_timeout_error", colType = SmallIntT }
        , sqlAddColumn $ tblColumn { colName     = "immediate_trash"
                                   , colType     = BoolT
                                   , colNullable = False
                                   , colDefault  = Just "false"
                                   }
        ]
  }

createTemporaryLoginTokensTable :: MonadDB m => Migration m
createTemporaryLoginTokensTable = Migration
  { mgrTableName = tblName tableTemporaryLoginTokens
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "temporary_login_tokens"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName     = "expiration_time"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "hash"
        , tblForeignKeys =
          [(fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }]
        }
  }

usersAddHomeFolderID :: MonadDB m => Migration m
usersAddHomeFolderID =
  let tname = tblName tableUsers
  in  Migration
        { mgrTableName = tname
        , mgrFrom      = 27
        , mgrAction    =
          StandardMigration $ do
            runQuery_ $ sqlAlterTable
              tname
              [ sqlAddColumn $ tblColumn { colName     = "home_folder_id"
                                         , colType     = BigIntT
                                         , colNullable = True
                                         }
              , sqlAddValidFK tname (fkOnColumn "home_folder_id" "folders" "id")
              ]
            runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "home_folder_id"
        }


addUserTotpIsMandatoryColumn :: MonadDB m => Migration m
addUserTotpIsMandatoryColumn = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 28
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableUsers)
                     [ sqlAddColumn $ tblColumn { colName     = "totp_is_mandatory"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                     ]
  }

addUserSysAuthColumn :: MonadDB m => Migration m
addUserSysAuthColumn = Migration
  { mgrTableName = tblName tableUsers
  , mgrFrom      = 29
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableUsers)
                     [ sqlAddColumn $ tblColumn { colName     = "sysauth"
                                                , colType     = SmallIntT
                                                , colNullable = False
                                                , colDefault  = Just "1"
                                                }
                     ]
  }
