module Flow.Tables (flowTables) where

import Database.PostgreSQL.PQTypes.Model

flowTables :: [Table]
flowTables =
  [ tableFlowTemplates
  , tableFlowCallbacks
  , tableFlowInstances
  , tableFlowInstanceKeyValueStore
  , tableFlowInstanceSignatories
  , tableFlowInstanceAccessTokens
  , tableFlowInstanceSessions
  , tableFlowEvents
  , tableFlowAggregatorEvents
  , tableFlowUserAuthConfigs
  , tableFlowEidAuthentications
  , tableFlowEidServiceTransactions
  ]

tableFlowTemplates :: Table
tableFlowTemplates = tblTable
  { tblName        = "flow_templates"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName     = "id"
                , colType     = UuidT
                , colNullable = False
                , colDefault  = Just "gen_random_uuid()"
                }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "process", colType = TextT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "folder_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "created", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "committed", colType = TimestampWithZoneT }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumn "user_id", indexOnColumn "folder_id"]
  , tblForeignKeys = [
    -- Do not allow to delete users or user groups that still contain templates.
                       fkOnColumn "user_id"   "users"   "id"
                     , fkOnColumn "folder_id" "folders" "id"
                     ]
  }

tableFlowInstances :: Table
tableFlowInstances = tblTable
  { tblName        = "flow_instances"
  , tblVersion     = 3
  , tblColumns     =
    [ tblColumn { colName     = "id"
                , colType     = UuidT
                , colNullable = False
                , colDefault  = Just "gen_random_uuid()"
                }
    , tblColumn { colName = "template_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "current_state", colType = TextT, colNullable = False }
    , tblColumn { colName = "started", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "title", colType = TextT, colNullable = True }
    , tblColumn { colName     = "last_event"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    , tblColumn { colName = "callback_id", colType = UuidT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumn "template_id"]
  , tblForeignKeys = [ fkOnColumn "template_id" "flow_templates" "id"
                     , fkOnColumn "callback_id" "flow_callbacks" "id"
                     ]
  }

tableFlowCallbacks :: Table
tableFlowCallbacks = tblTable
  { tblName       = "flow_callbacks"
  , tblVersion    = 1
  , tblColumns    = [ tblColumn { colName     = "id"
                                , colType     = UuidT
                                , colNullable = False
                                , colDefault  = Just "gen_random_uuid()"
                                }
                    , tblColumn { colName = "url", colType = TextT, colNullable = False }
                    , tblColumn { colName     = "version"
                                , colType     = IntegerT
                                , colNullable = False
                                }
                    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

tableFlowInstanceKeyValueStore :: Table
tableFlowInstanceKeyValueStore = tblTable
  { tblName        = "flow_instance_key_value_store"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "key", colType = TextT, colNullable = False }
    -- TODO: Column `type` should be an enum, see the check below for allowed values.
    , tblColumn { colName = "type", colType = TextT, colNullable = False }
    , tblColumn { colName = "string", colType = TextT, colNullable = True }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = True }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumns ["instance_id", "key"]
  , tblIndexes     =
                      -- Documents cannot be associated with multiple instances.
                     [ uniqueIndexOnColumn "document_id"
                      -- Users associated with an instance cannot be used for multiple keys.
                     , uniqueIndexOnColumns ["instance_id", "user_id"]
                     , indexOnColumn "string"
                     , indexOnColumn "user_id"
                     ]
  , tblForeignKeys =
    [ (fkOnColumn "instance_id" "flow_instances" "id") { fkOnDelete = ForeignKeyCascade }
    , fkOnColumn "document_id" "documents" "id"
    , fkOnColumn "user_id"     "users"     "id"
    ]
  , tblChecks      =
    [ tblCheck
        { chkName      = "check_value"
        , chkCondition =
          "type = 'document'::text AND document_id IS NOT NULL OR \
        \type = 'user'::text AND user_id IS NOT NULL OR \
        \type = 'email'::text AND string IS NOT NULL OR \
        \type = 'phone_number'::text AND string IS NOT NULL OR \
        \type = 'message'::text AND string IS NOT NULL"
        }
    ]
  }

tableFlowUserAuthConfigs :: Table
tableFlowUserAuthConfigs = tblTable
  { tblName        = "flow_user_auth_configs"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "key", colType = TextT, colNullable = False }
    , tblColumn { colName = "auth_to_view_provider", colType = TextT, colNullable = True }
    , tblColumn { colName     = "auth_to_view_max_failures"
                , colType     = IntegerT
                , colNullable = True
                }
    , tblColumn { colName     = "auth_to_view_archived_provider"
                , colType     = TextT
                , colNullable = True
                }
    , tblColumn { colName     = "auth_to_view_archived_max_failures"
                , colType     = IntegerT
                , colNullable = True
                }
    ]
  , tblPrimaryKey  = pkOnColumns ["instance_id", "key"]
  , tblIndexes     = []
  , tblForeignKeys = [ (fkOnColumns ["instance_id", "key"]
                                    "flow_instance_key_value_store"
                                    ["instance_id", "key"]
                       )
                         { fkOnDelete = ForeignKeyCascade
                         }
                     ]
  }

tableFlowInstanceSignatories :: Table
tableFlowInstanceSignatories = tblTable
  { tblName        = "flow_instance_signatories"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "signatory_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "key", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "signatory_id"
  , tblIndexes     = [indexOnColumns ["instance_id", "key"]]
  , tblForeignKeys = [ (fkOnColumns ["instance_id", "key"]
                                    "flow_instance_key_value_store"
                                    ["instance_id", "key"]
                       )
                       { fkOnDelete = ForeignKeyCascade
                       }
                     , fkOnColumn "signatory_id" "signatory_links" "id"
                     ]
  }

tableFlowInstanceAccessTokens :: Table
tableFlowInstanceAccessTokens = tblTable
  { tblName        = "flow_instance_access_tokens"
  , tblVersion     = 1
  , tblColumns = [ tblColumn { colName     = "id"
                             , colType     = UuidT
                             , colNullable = False
                             , colDefault  = Just "gen_random_uuid()"
                             }
    -- TODO: add expiration time?
                 , tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
                 , tblColumn { colName     = "instance_id"
                             , colType     = UuidT
                             , colNullable = False
                             }
                 , tblColumn { colName = "key", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumns ["instance_id", "key"]]
  , tblForeignKeys = [ (fkOnColumns ["instance_id", "key"]
                                    "flow_instance_key_value_store"
                                    ["instance_id", "key"]
                       )
                         { fkOnDelete = ForeignKeyCascade
                         }
                     ]
  }

-- | This table links Kontrakcja sessions to "Flow instance users", creating
-- "instance sessions". Instance sessions are created only for Flow users who
-- have authenticated with a valid invitation link.
tableFlowInstanceSessions :: Table
tableFlowInstanceSessions = tblTable
  { tblName        = "flow_instance_sessions"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "key", colType = TextT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumn "session_id"
  , tblIndexes     = [indexOnColumn "session_id"]
  , tblForeignKeys =
    [ (fkOnColumns ["instance_id", "key"]
                   "flow_instance_key_value_store"
                   ["instance_id", "key"]
      ) { fkOnDelete = ForeignKeyCascade
        }
    , (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableFlowEvents :: Table
tableFlowEvents = tblTable
  { tblName        = "flow_events"
  , tblVersion     = 3
  , tblColumns     =
    [ tblColumn { colName     = "id"
                , colType     = UuidT
                , colNullable = False
                , colDefault  = Just "gen_random_uuid()"
                }
    , tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "user_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "document_name", colType = TextT, colNullable = True }
    , tblColumn { colName = "user_action", colType = TextT, colNullable = False }
    , tblColumn { colName = "created", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "details", colType = JsonbT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumn "instance_id"]
  , tblForeignKeys =
    [(fkOnColumn "instance_id" "flow_instances" "id") { fkOnDelete = ForeignKeyCascade }]
  }

tableFlowAggregatorEvents :: Table
tableFlowAggregatorEvents = tblTable
  { tblName        = "flow_aggregator_events"
  , tblVersion     = 1
  , tblColumns     = [tblColumn { colName = "id", colType = UuidT, colNullable = False }]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblForeignKeys =
    [(fkOnColumn "id" "flow_events" "id") { fkOnDelete = ForeignKeyCascade }]
  }

-- todo: write test that checks that both flow_overview_authentications and
-- tableEIDAuthentications contain all `eid_authentication` columns. general
-- idea: authenticate overview, and push authentications to kontrakcja for each
-- document
tableFlowEidAuthentications :: Table
tableFlowEidAuthentications = tblTable
  { tblName        = "flow_eid_authentications"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "user_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "auth_kind", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "signature", colType = BinaryT }
    , tblColumn { colName = "signatory_name", colType = TextT }
    , tblColumn { colName = "signatory_personal_number", colType = TextT }
    , tblColumn { colName = "ocsp_response", colType = BinaryT }
    , tblColumn { colName = "internal_provider", colType = SmallIntT }
    , tblColumn { colName = "signatory_phone_number", colType = TextT }
    , tblColumn { colName = "signatory_date_of_birth", colType = TextT }
    , tblColumn { colName = "signatory_ip", colType = TextT }
    , tblColumn { colName = "signatory_email", colType = TextT }
    , tblColumn { colName = "provider_customer_id", colType = TextT }
    , tblColumn { colName = "provider_method", colType = TextT }
    ]
  , tblPrimaryKey  = pkOnColumns ["instance_id", "user_name", "auth_kind"]
  , tblForeignKeys =
    [ (fkOnColumns ["instance_id", "user_name"]
                   "flow_instance_key_value_store"
                   ["instance_id", "key"]
      ) { fkOnDelete = ForeignKeyCascade
        }
    , (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  }

tableFlowEidServiceTransactions :: Table
tableFlowEidServiceTransactions = tblTable
  { tblName        = "flow_eid_service_transactions"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "user_name", colType = TextT, colNullable = False }
    , tblColumn { colName = "auth_kind", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT }
    , tblColumn { colName = "transaction_id", colType = TextT, colNullable = False }
    , tblColumn { colName = "status", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName     = "deadline"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    ]
  -- only one authentication per participant. can be relaxed later if necessary.
  , tblPrimaryKey  = pkOnColumns ["instance_id", "user_name", "auth_kind"]
  , tblForeignKeys =
    [ (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumns ["instance_id", "user_name"]
                   "flow_instance_key_value_store"
                   ["instance_id", "key"]
      ) { fkOnDelete = ForeignKeyCascade
        }
    ]
  , tblIndexes     = [ indexOnColumn "session_id"
                     , (indexOnColumn "transaction_id") { idxUnique = True }
                     ]
  }
