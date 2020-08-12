module UserGroup.Tables where

import DB

tableUserGroups :: Table
tableUserGroups = tblTable
  { tblName        = "user_groups"
  , tblVersion     = 7
  , tblColumns     =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "parent_group_id", colType = BigIntT, colNullable = True }
    , tblColumn { colName    = "parent_group_path"
                , colType    = ArrayT BigIntT
                , colDefault = Just "ARRAY[]::bigint[]"
                }
    , tblColumn { colName = "name", colType = TextT }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    , tblColumn { colName = "home_folder_id", colType = BigIntT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [ indexOnColumnWithMethod "parent_group_path" GIN
                     , indexOnColumn "home_folder_id"
                     ]
  , tblForeignKeys =
    [
      -- do not allow to delete groups which still contains some other groups
      -- always must delete the child groups explicitely
      (fkOnColumn "parent_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyRestrict
                                                        }
    , (fkOnColumn "home_folder_id" "folders" "id") { fkOnDelete = ForeignKeyRestrict }
    ]
  }

tableUserGroupSettings :: Table
tableUserGroupSettings = tblTable
  { tblName        = "user_group_settings"
  , tblVersion     = 22
  , tblColumns     =
    [ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "ip_address_mask_list", colType = TextT }
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
    , tblColumn { colName     = "legal_text"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "idle_doc_timeout_preparation", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_closed", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_canceled", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_timedout", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_rejected", colType = SmallIntT }
    , tblColumn { colName = "idle_doc_timeout_error", colType = SmallIntT }
    , tblColumn { colName     = "immediate_trash"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "require_bpid_for_new_document"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "send_timeout_notification"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "use_folder_list_calls"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "totp_is_mandatory"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "session_timeout"
                , colType     = IntegerT
                , colNullable = True
                , colDefault  = Nothing
                }
    , tblColumn { colName = "portal_url", colType = TextT }
    , tblColumn { colName = "eid_service_token", colType = TextT }
    , tblColumn { colName     = "sealing_method"
                , colType     = SmallIntT
                , colNullable = False
                , colDefault  = Just "1"
                }
    , tblColumn { colName     = "document_session_timeout"
                , colType     = IntegerT
                , colNullable = True
                , colDefault  = Nothing
                }
    , tblColumn { colName     = "force_hide_pn"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "has_post_signview"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "sso_config", colType = JsonbT, colNullable = True }
    , tblColumn { colName     = "add_metadata_to_pdfs"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "use_eid_service_for_se_view"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "use_app_frontend"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName     = "sebankid_signing_override"
                , colType     = SmallIntT
                , colNullable = True
                , colDefault  = Nothing
                }
    , tblColumn { colName     = "pades_credentials_label"
                , colType     = TextT
                , colNullable = True
                , colDefault  = Nothing
                }
    ]
  , tblPrimaryKey  = pkOnColumn "user_group_id"
  , tblForeignKeys =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
  , tblIndexes     = [uniqueIndexOnColumn "(sso_config ->> 'idp_id'::text)"]
  }

-- Note: We only need 2 last versions of the ctUserGroupSettings composite type.
-- When adding a new version remove the oldest one below and drop the type from the db
-- in your migration code.
ctUserGroupSettings :: CompositeType
ctUserGroupSettings = CompositeType
  { ctName    = "user_group_settings_c16"
  , ctColumns =
    [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
    , CompositeColumn { ccName = "idle_doc_timeout_preparation", ccType = SmallIntT }
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
    , CompositeColumn { ccName = "document_session_timeout", ccType = IntegerT }
    , CompositeColumn { ccName = "force_hide_pn", ccType = BoolT }
    , CompositeColumn { ccName = "has_post_signview", ccType = BoolT }
    , CompositeColumn { ccName = "sso_config", ccType = JsonbT }
    , CompositeColumn { ccName = "add_metadata_to_pdfs", ccType = BoolT }
    , CompositeColumn { ccName = "use_eid_service_for_se_view", ccType = BoolT }
    , CompositeColumn { ccName = "use_app_frontend", ccType = BoolT }
    , CompositeColumn { ccName = "sebankid_signing_override", ccType = SmallIntT }
    , CompositeColumn { ccName = "pades_credentials_label", ccType = TextT }
    ]
  }

ctUserGroupSettings15 :: CompositeType
ctUserGroupSettings15 = CompositeType
  { ctName    = "user_group_settings_c15"
  , ctColumns =
    [ CompositeColumn { ccName = "ip_address_mask_list", ccType = TextT }
    , CompositeColumn { ccName = "idle_doc_timeout_preparation", ccType = SmallIntT }
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
    , CompositeColumn { ccName = "document_session_timeout", ccType = IntegerT }
    , CompositeColumn { ccName = "force_hide_pn", ccType = BoolT }
    , CompositeColumn { ccName = "has_post_signview", ccType = BoolT }
    , CompositeColumn { ccName = "sso_config", ccType = JsonbT }
    , CompositeColumn { ccName = "add_metadata_to_pdfs", ccType = BoolT }
    , CompositeColumn { ccName = "use_eid_service_for_se_view", ccType = BoolT }
    , CompositeColumn { ccName = "use_app_frontend", ccType = BoolT }
    , CompositeColumn { ccName = "sebankid_signing_override", ccType = SmallIntT }
    ]
  }

tableUserGroupAddresses :: Table
tableUserGroupAddresses = tblTable
  { tblName        = "user_group_addresses"
  , tblVersion     = 2
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
                     , tblColumn { colName     = "entity_name"
                                 , colType     = TextT
                                 , colNullable = False
                                 , colDefault  = Just "''::text"
                                 }
                     ]
  , tblPrimaryKey  = pkOnColumn "user_group_id"
  , tblForeignKeys =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
  }

ctUserGroupAddress :: CompositeType
ctUserGroupAddress = CompositeType
  { ctName    = "user_group_address_c2"
  , ctColumns = [ CompositeColumn { ccName = "company_number", ccType = TextT }
                , CompositeColumn { ccName = "entity_name", ccType = TextT }
                , CompositeColumn { ccName = "address", ccType = TextT }
                , CompositeColumn { ccName = "zip", ccType = TextT }
                , CompositeColumn { ccName = "city", ccType = TextT }
                , CompositeColumn { ccName = "country", ccType = TextT }
                ]
  }

ctUserGroupAddress1 :: CompositeType
ctUserGroupAddress1 = CompositeType
  { ctName    = "user_group_address_c1"
  , ctColumns = [ CompositeColumn { ccName = "company_number", ccType = TextT }
                , CompositeColumn { ccName = "address", ccType = TextT }
                , CompositeColumn { ccName = "zip", ccType = TextT }
                , CompositeColumn { ccName = "city", ccType = TextT }
                , CompositeColumn { ccName = "country", ccType = TextT }
                ]
  }

tableUserGroupUIs :: Table
tableUserGroupUIs = tblTable
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
    [ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
    , fkOnColumn "mail_theme"     "themes" "id"
    , fkOnColumn "signview_theme" "themes" "id"
    , fkOnColumn "service_theme"  "themes" "id"
    ]
  }

ctUserGroupUI :: CompositeType
ctUserGroupUI = CompositeType
  { ctName    = "user_group_ui_c1"
  , ctColumns = [ CompositeColumn { ccName = "mail_theme", ccType = BigIntT }
                , CompositeColumn { ccName = "signview_theme", ccType = BigIntT }
                , CompositeColumn { ccName = "service_theme", ccType = BigIntT }
                , CompositeColumn { ccName = "browser_title", ccType = TextT }
                , CompositeColumn { ccName = "sms_originator", ccType = TextT }
                , CompositeColumn { ccName = "favicon", ccType = BinaryT }
                ]
  }

tableUserGroupInvoicings :: Table
tableUserGroupInvoicings = tblTable
  { tblName        = "user_group_invoicings"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "invoicing_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "payment_plan", colType = SmallIntT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumn "user_group_id"
  , tblForeignKeys =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
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

ctUserGroupInvoicing :: CompositeType
ctUserGroupInvoicing = CompositeType
  { ctName    = "user_group_invoicing_c1"
  , ctColumns = [ CompositeColumn { ccName = "invoicing_type", ccType = SmallIntT }
                , CompositeColumn { ccName = "payment_plan", ccType = SmallIntT }
                ]
  }

tableUserGroupFreeDocumentTokens :: Table
tableUserGroupFreeDocumentTokens = tblTable
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
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
  }

---------------------------------

tableUserGroupTags :: Table
tableUserGroupTags = tblTable
  { tblName        = "user_group_tags"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "value", colType = TextT, colNullable = False }
    , tblColumn { colName = "internal", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["user_group_id", "name", "internal"]
  , tblIndexes     = [indexOnColumns ["value", "internal"]]
  , tblForeignKeys =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
  }

tableUserGroupDeletionRequests :: Table
tableUserGroupDeletionRequests = tblTable
  { tblName        = "user_group_deletion_requests"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "for_user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName     = "requested_by_user_id"
                , colType     = BigIntT
                , colNullable = False
                }
    , tblColumn { colName     = "signed_off_by_user_id"
                , colType     = BigIntT
                , colNullable = True
                }
    , tblColumn { colName     = "requested_deletion_date"
                , colType     = DateT
                , colNullable = False
                }
    , tblColumn { colName = "expires", colType = TimestampWithZoneT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumns ["for_user_group_id"]
  , tblIndexes     =
    [ indexOnColumns
        ["for_user_group_id", "requested_by_user_id", "signed_off_by_user_id"]
    ]
  , tblForeignKeys =
    [ (fkOnColumn "for_user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade
                                                          }
    , (fkOnColumn "requested_by_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signed_off_by_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableUserGroupDeletionLog :: Table
tableUserGroupDeletionLog = tblTable
  { tblName        = "user_group_deletion_log"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName     = "deleted_user_group_id"
                , colType     = BigIntT
                , colNullable = False
                }
    , tblColumn { colName     = "requested_by_user_id"
                , colType     = BigIntT
                , colNullable = False
                }
    , tblColumn { colName     = "signed_off_by_user_id"
                , colType     = BigIntT
                , colNullable = False
                }
    , tblColumn { colName     = "requested_deletion_date"
                , colType     = DateT
                , colNullable = False
                }
    , tblColumn { colName     = "deletion_time"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    ]
  , tblPrimaryKey  = pkOnColumns ["deleted_user_group_id"]
  , tblIndexes     =
    [ indexOnColumns
        ["deleted_user_group_id", "requested_by_user_id", "signed_off_by_user_id"]
    ]
  , tblForeignKeys =
    [ (fkOnColumn "requested_by_user_id" "users" "id") { fkOnDelete = ForeignKeyRestrict }
    , (fkOnColumn "signed_off_by_user_id" "users" "id") { fkOnDelete = ForeignKeyRestrict
                                                        }
    ]
  }
