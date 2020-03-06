module UserGroup.Migrations where

import Data.Int
import Database.PostgreSQL.PQTypes.Checks

import DB
import UserGroup.Tables

renameUserGroupComposites :: MonadDB m => Migration m
renameUserGroupComposites = Migration
  { mgrTableName = "user_groups"
  , mgrFrom      = 6
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlDropComposite "user_group" -- not needed
      runSQL_ "ALTER TYPE user_group_setting RENAME TO user_group_settings_c1"
      runSQL_ "ALTER TYPE user_group_address RENAME TO user_group_address_c1"
      runSQL_ "ALTER TYPE user_group_ui RENAME TO user_group_ui_c1"
      runSQL_ "ALTER TYPE user_group_invoicing RENAME TO user_group_invoicing_c1"
  }

createTableUserGroups :: MonadDB m => Migration m
createTableUserGroups = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "user_groups"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName     = "parent_group_id"
                      , colType     = BigIntT
                      , colNullable = True
                      }
          , tblColumn { colName    = "parent_group_path"
                      , colType    = ArrayT BigIntT
                      , colDefault = Just "ARRAY[]::bigint[]"
                      }
          , tblColumn { colName = "name", colType = TextT }
          ]
        , tblPrimaryKey  = pkOnColumn "id"
        , tblForeignKeys =
          [
          -- do not allow to delete groups which still contains some other groups
          -- always must delete the child groups explicitely
            (fkOnColumn "parent_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyRestrict
                                                              }
          ]
        }
  }

createTableUserGroupSettings :: MonadDB m => Migration m
createTableUserGroupSettings = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "user_group_settings"
        , tblVersion     = 1
        , tblColumns = [ tblColumn { colName     = "user_group_id"
                                   , colType     = BigIntT
                                   , colNullable = False
                                   }
                       , tblColumn { colName = "ip_address_mask_list", colType = TextT }
                       , tblColumn { colName = "idle_doc_timeout", colType = SmallIntT }
                       , tblColumn { colName = "cgi_display_name", colType = TextT }
                       , tblColumn { colName     = "sms_provider"
                                   , colType     = SmallIntT
                                   , colNullable = False
                                   , colDefault  = Just "1"
                                   }
                       , tblColumn { colName = "cgi_service_id", colType = TextT }
                       , tblColumn { colName     = "pad_app_mode"
                                   , colType     = SmallIntT
                                   , colNullable = False
                                   , colDefault  = Just "1"
                                   }
                       , tblColumn { colName     = "pad_earchive_enabled"
                                   , colType     = BoolT
                                   , colNullable = False
                                   , colDefault  = Just "true"
                                   }
                       ]
        , tblPrimaryKey  = pkOnColumn "user_group_id"
        , tblForeignKeys =
          [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                            }
          ]
        }
  }

createTableUserGroupAddresses :: MonadDB m => Migration m
createTableUserGroupAddresses = Migration
  { mgrTableName = tblName tableUserGroupAddresses
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "user_group_addresses"
        , tblVersion     = 1
        , tblColumns     = [ tblColumn { colName     = "user_group_id"
                                       , colType     = BigIntT
                                       , colNullable = False
                                       }
                           , tblColumn { colName     = "company_number"
                                       , colType     = TextT
                                       , colNullable = False
                                       , colDefault  = Just "''::text"
                                       }
                           , tblColumn { colName     = "address"
                                       , colType     = TextT
                                       , colNullable = False
                                       , colDefault  = Just "''::text"
                                       }
                           , tblColumn { colName     = "zip"
                                       , colType     = TextT
                                       , colNullable = False
                                       , colDefault  = Just "''::text"
                                       }
                           , tblColumn { colName     = "city"
                                       , colType     = TextT
                                       , colNullable = False
                                       , colDefault  = Just "''::text"
                                       }
                           , tblColumn { colName     = "country"
                                       , colType     = TextT
                                       , colNullable = False
                                       , colDefault  = Just "''::text"
                                       }
                           ]
        , tblPrimaryKey  = pkOnColumn "user_group_id"
        , tblForeignKeys =
          [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                            }
          ]
        }
  }

createTableUserGroupUIs :: MonadDB m => Migration m
createTableUserGroupUIs = Migration
  { mgrTableName = tblName tableUserGroupUIs
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "user_group_uis"
        , tblVersion     = 1
        , tblColumns     = [ tblColumn { colName     = "user_group_id"
                                       , colType     = BigIntT
                                       , colNullable = False
                                       }
                           , tblColumn { colName = "mail_theme", colType = BigIntT }
                           , tblColumn { colName = "signview_theme", colType = BigIntT }
                           , tblColumn { colName = "service_theme", colType = BigIntT }
                           , tblColumn { colName = "browser_title", colType = TextT }
                           , tblColumn { colName = "sms_originator", colType = TextT }
                           , tblColumn { colName = "favicon", colType = BinaryT }
                           ]
        , tblPrimaryKey  = pkOnColumn "user_group_id"
        , tblForeignKeys =
          [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                            }
          , (fkOnColumn "mail_theme" "themes" "id")
          , (fkOnColumn "signview_theme" "themes" "id")
          , (fkOnColumn "service_theme" "themes" "id")
          ]
        }
  }

createTableUserGroupInvoicings :: MonadDB m => Migration m
createTableUserGroupInvoicings = Migration
  { mgrTableName = tblName tableUserGroupInvoicings
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "user_group_invoicings"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName     = "user_group_id"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName     = "invoicing_type"
                      , colType     = SmallIntT
                      , colNullable = False
                      }
          , tblColumn { colName     = "payment_plan"
                      , colType     = SmallIntT
                      , colNullable = True
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "user_group_id"
        , tblForeignKeys =
          [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                            }
          ]
        , tblChecks      =
          [ tblCheck
              { chkName      = "user_group_invoicing_type_matches_payplan"
              , chkCondition =
                "invoicing_type = 1 AND payment_plan IS NULL \
            \OR invoicing_type = 2 \
            \OR invoicing_type = 3 AND payment_plan IS NOT NULL"
              }
          ]
        }
  }

userGroupsAdjustIDSequence :: MonadDB m => Migration m
userGroupsAdjustIDSequence = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom      = 1
  , mgrAction    = StandardMigration
                     $ runSQL_ "SELECT setval('user_groups_id_seq', max(id)) FROM companies"
  }

usergroupsBumpVersionAfterDroppingCompanies :: MonadDB m => Migration m
usergroupsBumpVersionAfterDroppingCompanies = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ return ()
  }

usergroupsAddDeleted :: MonadDB m => Migration m
usergroupsAddDeleted = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom      = 3
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableUserGroups)
                     [ sqlAddColumn
                         $ tblColumn { colName = "deleted", colType = TimestampWithZoneT }
                     ]
  }

userGroupSettingsAddLegalText :: MonadDB m => Migration m
userGroupSettingsAddLegalText = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableUserGroupSettings)
                     [ sqlAddColumn $ tblColumn { colName     = "legal_text"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                     ]
  }

userGroupSettingsSplitIdleDocTimeout :: MonadDB m => Migration m
userGroupSettingsSplitIdleDocTimeout = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 2
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
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
        ]

      void
        $ runSQL
            "UPDATE user_group_settings SET\
       \ idle_doc_timeout_preparation = idle_doc_timeout,\
       \ idle_doc_timeout_closed      = idle_doc_timeout,\
       \ idle_doc_timeout_canceled    = idle_doc_timeout,\
       \ idle_doc_timeout_timedout    = idle_doc_timeout,\
       \ idle_doc_timeout_rejected    = idle_doc_timeout,\
       \ idle_doc_timeout_error       = idle_doc_timeout"

      runQuery_ $ sqlAlterTable (tblName tableUserGroupSettings)
                                [sqlDropColumn "idle_doc_timeout"]
  }

userGroupSettingsAddImmediateTrash :: MonadDB m => Migration m
userGroupSettingsAddImmediateTrash = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 3
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableUserGroupSettings)
                       [ sqlAddColumn $ tblColumn { colName     = "immediate_trash"
                                                  , colType     = BoolT
                                                  , colNullable = False
                                                  , colDefault  = Just "false"
                                                  }
                       ]
  }

userGroupAddGINIdx :: MonadDB m => Migration m
userGroupAddGINIdx = Migration
  { mgrTableName = tblName tableUserGroups
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     runQuery_
                       . sqlCreateIndexSequentially (tblName tableUserGroups)
                       $ (indexOnColumnWithMethod "parent_group_path" GIN)
  }

addUserGroupHomeFolderID :: MonadDB m => Migration m
addUserGroupHomeFolderID =
  let tname = tblName tableUserGroups
  in  Migration
        { mgrTableName = tname
        , mgrFrom      = 5
        , mgrAction    =
          StandardMigration $ do
            runQuery_
              . sqlAlterTable tname
              $ [ sqlAddColumn tblColumn { colName     = "home_folder_id"
                                         , colType     = BigIntT
                                         , colNullable = True
                                         }
                , sqlAddValidFK tname $ (fkOnColumn "home_folder_id" "folders" "id")
                  { fkOnDelete = ForeignKeyRestrict
                  }
                ]
            runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "home_folder_id"
        }

userGroupSettingsAddRequireBPIDForNewDocument :: MonadDB m => Migration m
userGroupSettingsAddRequireBPIDForNewDocument = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableUserGroupSettings)
                       [ sqlAddColumn $ tblColumn { colName = "require_bpid_for_new_document"
                                                  , colType     = BoolT
                                                  , colNullable = False
                                                  , colDefault  = Just "false"
                                                  }
                       ]
  }

userGroupSettingsAddSendTimeoutNotification :: MonadDB m => Migration m
userGroupSettingsAddSendTimeoutNotification = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 5
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn { colName     = "send_timeout_notification"
                                   , colType     = BoolT
                                   , colNullable = False
                                   , colDefault  = Just "false"
                                   }
        ]
      runQuery_ $ sqlDropComposite $ "user_group_settings_c1"
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          ]
        }
  }

userGroupSettingsAddFolderListCallFlag :: MonadDB m => Migration m
userGroupSettingsAddFolderListCallFlag = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 6
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c2"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          ]
        }
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn { colName     = "use_folder_list_calls"
                                   , colType     = BoolT
                                   , colNullable = False
                                   , colDefault  = Just "false"
                                   }
        ]
  }

userGroupSettingsAddTotpIsMandatory :: MonadDB m => Migration m
userGroupSettingsAddTotpIsMandatory = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 7
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn { colName     = "totp_is_mandatory"
                                   , colType     = BoolT
                                   , colNullable = False
                                   , colDefault  = Just "false"
                                   }
        ]
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c3"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          , CompositeColumn { ccName = "totp_is_mandatory", ccType = BoolT }
          ]
        }
  }

userGroupSettingsAddSessionTimeout :: MonadDB m => Migration m
userGroupSettingsAddSessionTimeout = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 8
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn { colName     = "session_timeout"
                                   , colType     = IntegerT
                                   , colNullable = True
                                   , colDefault  = Nothing
                                   }
        ]

      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c4"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          , CompositeColumn { ccName = "totp_is_mandatory", ccType = BoolT }
          , CompositeColumn { ccName = "session_timeout", ccType = IntegerT }
          ]
        }
  }

userGroupSettingsAddPortalUrl :: MonadDB m => Migration m
userGroupSettingsAddPortalUrl = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 9
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [sqlAddColumn $ tblColumn { colName = "portal_url", colType = TextT }]
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c5"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          , CompositeColumn { ccName = "totp_is_mandatory", ccType = BoolT }
          , CompositeColumn { ccName = "session_timeout", ccType = IntegerT }
          , CompositeColumn { ccName = "portal_url", ccType = TextT }
          ]
        }
  }

userGroupAddressAddEntityNameField :: MonadDB m => Migration m
userGroupAddressAddEntityNameField = Migration
  { mgrTableName = tblName tableUserGroupAddresses
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupAddresses)
        [ sqlAddColumn $ tblColumn { colName     = "entity_name"
                                   , colType     = TextT
                                   , colNullable = False
                                   , colDefault  = Just "''::text"
                                   }
        ]
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_address_c2"
        , ctColumns = [ CompositeColumn { ccName = "company_number", ccType = TextT }
                      , CompositeColumn { ccName = "entity_name", ccType = TextT }
                      , CompositeColumn { ccName = "address", ccType = TextT }
                      , CompositeColumn { ccName = "zip", ccType = TextT }
                      , CompositeColumn { ccName = "city", ccType = TextT }
                      , CompositeColumn { ccName = "country", ccType = TextT }
                      ]
        }
      -- Populate Entity Name from the User Group name
      runQuery_ $ sqlUpdate "user_group_addresses" $ do
        sqlSetCmd "entity_name" "user_groups.name"
        sqlFrom "user_groups"
        sqlWhere "user_group_addresses.user_group_id = user_groups.id"
  }

userGroupSettingsAddEidServiceToken :: MonadDB m => Migration m
userGroupSettingsAddEidServiceToken = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 10
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [sqlAddColumn $ tblColumn { colName = "eid_service_token", colType = TextT }]
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c6"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          , CompositeColumn { ccName = "totp_is_mandatory", ccType = BoolT }
          , CompositeColumn { ccName = "session_timeout", ccType = IntegerT }
          , CompositeColumn { ccName = "portal_url", ccType = TextT }
          , CompositeColumn { ccName = "eid_service_token", ccType = TextT }
          ]
        }
  }


createTableUserGroupFreeDocumentTokens :: MonadDB m => Migration m
createTableUserGroupFreeDocumentTokens = Migration
  { mgrTableName = "user_group_free_document_tokens"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ do
      createTable
        True
        tblTable
          { tblName        = "user_group_free_document_tokens"
          , tblVersion     = 1
          , tblColumns     = [ tblColumn { colName     = "user_group_id"
                                         , colType     = BigIntT
                                         , colNullable = False
                                         }
                             , tblColumn { colName     = "tokens_count"
                                         , colType     = IntegerT
                                         , colNullable = False
                                         }
                             , tblColumn { colName     = "tokens_validity"
                                         , colType     = TimestampWithZoneT
                                         , colNullable = False
                                         }
                             ]
          , tblPrimaryKey  = pkOnColumn "user_group_id"
          , tblChecks      = [ tblCheck { chkName      = "user_group_positive_count"
                                        , chkCondition = "tokens_count >= 0"
                                        }
                             ]
          , tblForeignKeys =
            [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
            ]
          }
      -- After migration we want free accounts to keep working for a while.
      -- Exact values set here don't matter so much as we will adjust this with marketing few days
      -- after this code is on production
      runQuery_
        . sqlInsertSelect "user_group_free_document_tokens" "user_group_invoicings as i"
        $ do
            sqlSetCmd "user_group_id" "i.user_group_id"
            sqlSet "tokens_count" (5 :: Int32)
            sqlSetCmd "tokens_validity" "now() + interval '30 days'"
            sqlWhereEq "i.invoicing_type" (3 :: Int16) -- User groups on individual invoicing
            sqlWhereEq "i.payment_plan"   (0 :: Int16) -- Free plan
  }

createTableUserGroupTags :: MonadDB m => Migration m
createTableUserGroupTags = Migration
  { mgrTableName = tblName tableUserGroupTags
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ do
      createTable
        True
        tblTable
          { tblName        = "user_group_tags"
          , tblVersion     = 1
          , tblColumns     =
            [ tblColumn { colName     = "user_group_id"
                        , colType     = BigIntT
                        , colNullable = False
                        }
            , tblColumn { colName = "name", colType = TextT, colNullable = False }
            , tblColumn { colName = "value", colType = TextT, colNullable = False }
            , tblColumn { colName = "internal", colType = BoolT, colNullable = False }
            ]
          , tblPrimaryKey  = pkOnColumns ["user_group_id", "name", "internal"]
          , tblIndexes     = [indexOnColumns ["value", "internal"]]
          , tblForeignKeys =
            [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
            ]
          }
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_tag_c1"
        , ctColumns = [ CompositeColumn { ccName = "name", ccType = TextT }
                      , CompositeColumn { ccName = "value", ccType = TextT }
                      ]
        }
  }

renameUserGroupTagComposite :: MonadDB m => Migration m
renameUserGroupTagComposite = Migration
  { mgrTableName = tblName tableUserGroupTags
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlDropComposite "user_group_tag_c1"
                     runQuery_ $ sqlCreateComposite $ CompositeType
                       { ctName = "tag_c1"
                       , ctColumns = [ CompositeColumn { ccName = "name", ccType = TextT }
                                     , CompositeColumn { ccName = "value", ccType = TextT }
                                     ]
                       }
  }

userGroupSettingsAddSealingMethod :: MonadDB m => Migration m
userGroupSettingsAddSealingMethod = Migration
  { mgrTableName = tblName tableUserGroupSettings
  , mgrFrom      = 11
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUserGroupSettings)
        [ sqlAddColumn $ tblColumn { colName     = "sealing_method"
                                   , colType     = SmallIntT
                                   , colNullable = False
                                   , colDefault  = Just "1"
                                   }
        ]
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "user_group_settings_c7"
        , ctColumns =
          [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
          , CompositeColumn { ccName = "idle_doc_timeout_preparation"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "idle_doc_timeout_closed", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_canceled", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_timedout", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_rejected", ccType = SmallIntT }
          , CompositeColumn { ccName = "idle_doc_timeout_error", ccType = SmallIntT }
          , CompositeColumn { ccName = "immediate_trash", ccType = BoolT }
          , CompositeColumn { ccName = "cgi_display_name", ccType = TextT }
          , CompositeColumn { ccName = "sms_provider", ccType = SmallIntT }
          , CompositeColumn { ccName = "cgi_service_id", ccType = TextT }
          , CompositeColumn { ccName = "pad_app_mode", ccType = SmallIntT }
          , CompositeColumn { ccName = "pad_earchive_enabled", ccType = BoolT }
          , CompositeColumn { ccName = "legal_text", ccType = BoolT }
          , CompositeColumn { ccName = "require_bpid_for_new_document", ccType = BoolT }
          , CompositeColumn { ccName = "send_timeout_notification", ccType = BoolT }
          , CompositeColumn { ccName = "use_folder_list_calls", ccType = BoolT }
          , CompositeColumn { ccName = "totp_is_mandatory", ccType = BoolT }
          , CompositeColumn { ccName = "session_timeout", ccType = IntegerT }
          , CompositeColumn { ccName = "portal_url", ccType = TextT }
          , CompositeColumn { ccName = "eid_service_token", ccType = TextT }
          , CompositeColumn { ccName = "sealing_method", ccType = SmallIntT }
          ]
        }
  }
