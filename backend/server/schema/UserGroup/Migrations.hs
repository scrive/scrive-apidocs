module UserGroup.Migrations where

import Database.PostgreSQL.PQTypes.Checks

import DB
import UserGroup.Tables

createTableUserGroups :: MonadDB m => Migration m
createTableUserGroups = Migration {
    mgrTableName = tblName tableUserGroups
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "user_groups"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "parent_group_id", colType = BigIntT, colNullable = True }
      , tblColumn { colName = "parent_group_path", colType = ArrayT BigIntT, colDefault = Just "ARRAY[]::bigint[]" }
      , tblColumn { colName = "name", colType = TextT }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblForeignKeys = [
          -- do not allow to delete groups which still contains some other groups
          -- always must delete the child groups explicitely
          (fkOnColumn "parent_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyRestrict }
        ]
    }
  }

createTableUserGroupSettings :: MonadDB m => Migration m
createTableUserGroupSettings = Migration {
    mgrTableName = tblName tableUserGroupSettings
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "user_group_settings"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "ip_address_mask_list", colType = TextT }
      , tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }
      , tblColumn { colName = "cgi_display_name", colType = TextT }
      , tblColumn { colName = "sms_provider", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
      , tblColumn { colName = "cgi_service_id", colType = TextT }
      , tblColumn { colName = "pad_app_mode", colType = SmallIntT, colNullable = False, colDefault = Just "1"}
      , tblColumn { colName = "pad_earchive_enabled", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
    , tblPrimaryKey = pkOnColumn "user_group_id"
    , tblForeignKeys = [
        (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
      ]
    }
  }

createTableUserGroupAddresses :: MonadDB m => Migration m
createTableUserGroupAddresses = Migration {
    mgrTableName = tblName tableUserGroupAddresses
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "user_group_addresses"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "company_number", colType = TextT, colNullable = False, colDefault = Just "''::text" }
      , tblColumn { colName = "address", colType = TextT, colNullable = False, colDefault = Just "''::text" }
      , tblColumn { colName = "zip", colType = TextT, colNullable = False, colDefault = Just "''::text" }
      , tblColumn { colName = "city", colType = TextT, colNullable = False, colDefault = Just "''::text" }
      , tblColumn { colName = "country", colType = TextT, colNullable = False, colDefault = Just "''::text" }
      ]
    , tblPrimaryKey = pkOnColumn "user_group_id"
    , tblForeignKeys = [
        (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
      ]
    }
  }

createTableUserGroupUIs :: MonadDB m => Migration m
createTableUserGroupUIs = Migration {
    mgrTableName = tblName tableUserGroupUIs
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "user_group_uis"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "user_group_id",  colType = BigIntT, colNullable = False }
      , tblColumn { colName = "mail_theme",     colType = BigIntT}
      , tblColumn { colName = "signview_theme", colType = BigIntT}
      , tblColumn { colName = "service_theme",  colType = BigIntT}
      , tblColumn { colName = "browser_title",  colType = TextT}
      , tblColumn { colName = "sms_originator", colType = TextT}
      , tblColumn { colName = "favicon",        colType = BinaryT}
      ]
    , tblPrimaryKey = pkOnColumn "user_group_id"
    , tblForeignKeys = [
        (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade },
        (fkOnColumn "mail_theme" "themes" "id"),
        (fkOnColumn "signview_theme" "themes" "id"),
        (fkOnColumn "service_theme" "themes" "id")
      ]
    }
  }

createTableUserGroupInvoicings :: MonadDB m => Migration m
createTableUserGroupInvoicings = Migration {
    mgrTableName = tblName tableUserGroupInvoicings
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "user_group_invoicings"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "user_group_id",  colType = BigIntT, colNullable = False }
      , tblColumn { colName = "invoicing_type", colType = SmallIntT, colNullable = False}
      , tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = True}
      ]
    , tblPrimaryKey = pkOnColumn "user_group_id"
    , tblForeignKeys = [
        (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
      ]
    , tblChecks =
      [ tblCheck
        { chkName = "user_group_invoicing_type_matches_payplan"
        , chkCondition =
               "invoicing_type = 1 AND payment_plan IS NULL \
            \OR invoicing_type = 2 \
            \OR invoicing_type = 3 AND payment_plan IS NOT NULL"
        }
      ]
    }
  }

userGroupsAdjustIDSequence :: MonadDB m => Migration m
userGroupsAdjustIDSequence = Migration {
    mgrTableName = tblName tableUserGroups
  , mgrFrom = 1
  , mgrAction = StandardMigration $
      runSQL_ "SELECT setval('user_groups_id_seq', max(id)) FROM companies"
  }

usergroupsBumpVersionAfterDroppingCompanies :: MonadDB m => Migration m
usergroupsBumpVersionAfterDroppingCompanies = Migration {
    mgrTableName = tblName tableUserGroups
  , mgrFrom = 2
  , mgrAction = StandardMigration $ return ()
  }

usergroupsAddDeleted :: MonadDB m => Migration m
usergroupsAddDeleted = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom = 3
  , mgrAction = StandardMigration $
      runQuery_ $ sqlAlterTable (tblName tableUserGroups)
        [ sqlAddColumn $
            tblColumn { colName = "deleted", colType = TimestampWithZoneT }
        ]
  }

userGroupSettingsAddLegalText :: MonadDB m => Migration m
userGroupSettingsAddLegalText = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom = 1
  , mgrAction = StandardMigration $
      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn
            { colName     = "legal_text"
            , colType     = BoolT
            , colNullable = False
            , colDefault  = Just "false"
            }
        ]
  }

userGroupSettingsSplitIdleDocTimeout :: MonadDB m => Migration m
userGroupSettingsSplitIdleDocTimeout = Migration {
    mgrTableName = tblName tableUserGroupSettings
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_preparation", colType = SmallIntT }
        , sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_closed", colType = SmallIntT }
        , sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_canceled", colType = SmallIntT }
        , sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_timedout", colType = SmallIntT }
        , sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_rejected", colType = SmallIntT }
        , sqlAddColumn $ tblColumn
            { colName = "idle_doc_timeout_error", colType = SmallIntT }
        ]

      void $ runSQL
       "UPDATE user_group_settings SET\
       \ idle_doc_timeout_preparation = idle_doc_timeout,\
       \ idle_doc_timeout_closed      = idle_doc_timeout,\
       \ idle_doc_timeout_canceled    = idle_doc_timeout,\
       \ idle_doc_timeout_timedout    = idle_doc_timeout,\
       \ idle_doc_timeout_rejected    = idle_doc_timeout,\
       \ idle_doc_timeout_error       = idle_doc_timeout"

      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
        [ sqlDropColumn "idle_doc_timeout" ]
  }

userGroupSettingsAddImmediateTrash :: MonadDB m => Migration m
userGroupSettingsAddImmediateTrash = Migration {
    mgrTableName = tblName tableUserGroupSettings
  , mgrFrom = 3
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn
            { colName     = "immediate_trash"
            , colType     = BoolT
            , colNullable = False
            , colDefault  = Just "false"
            }
        ]
  }

userGroupAddGINIdx :: MonadDB m => Migration m
userGroupAddGINIdx = Migration {
    mgrTableName = tblName tableUserGroups
  , mgrFrom = 4
  , mgrAction = StandardMigration $ do
      runQuery_ . sqlCreateIndexSequentially (tblName tableUserGroups) $
                  (indexOnColumnWithMethod "parent_group_path" GIN)
  }

addUserGroupHomeFolderID :: MonadDB m => Migration m
addUserGroupHomeFolderID =
  let tname = tblName tableUserGroups
  in Migration
     { mgrTableName = tname
     , mgrFrom = 5
     , mgrAction = StandardMigration $ do
         runQuery_ . sqlAlterTable tname $
           [ sqlAddColumn tblColumn
             { colName = "home_folder_id", colType = BigIntT
             , colNullable = True }
           , sqlAddValidFK tname $ (fkOnColumn "home_folder_id" "folders" "id")
             { fkOnDelete = ForeignKeyRestrict }
           ]
         runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "home_folder_id"
     }


userGroupSettingsAddRequireBPIDForNewDocument :: MonadDB m => Migration m
userGroupSettingsAddRequireBPIDForNewDocument = Migration {
    mgrTableName = tblName tableUserGroupSettings
  , mgrFrom = 4
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn
            { colName     = "require_bpid_for_new_document"
            , colType     = BoolT
            , colNullable = False
            , colDefault  = Just "false"
            }
        ]
  }
