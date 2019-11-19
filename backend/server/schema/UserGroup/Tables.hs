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
  , tblVersion     = 11
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
    ]
  , tblPrimaryKey  = pkOnColumn "user_group_id"
  , tblForeignKeys =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }]
  }


ctUserGroupSettings :: CompositeType
ctUserGroupSettings = CompositeType
  { ctName    = "user_group_settings_c6"
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
    ]
  }

ctUserGroupSettings5 :: CompositeType
ctUserGroupSettings5 = CompositeType
  { ctName    = "user_group_settings_c5"
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
    ]
  }

ctUserGroupSettings4 :: CompositeType
ctUserGroupSettings4 = CompositeType
  { ctName    = "user_group_settings_c4"
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
    ]
  }

ctUserGroupSettings3 :: CompositeType
ctUserGroupSettings3 = CompositeType
  { ctName    = "user_group_settings_c3"
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
    ]
  }

ctUserGroupSettings2 :: CompositeType
ctUserGroupSettings2 = CompositeType
  { ctName    = "user_group_settings_c2"
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
    ]
  }

ctUserGroupSettings1 :: CompositeType
ctUserGroupSettings1 = CompositeType
  { ctName    = "user_group_settings_c1"
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
    , (fkOnColumn "mail_theme" "themes" "id")
    , (fkOnColumn "signview_theme" "themes" "id")
    , (fkOnColumn "service_theme" "themes" "id")
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
