module Flow.Migrations where

import Data.Yaml
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Model
import Log
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE

import DB
import Flow.Id
import Flow.Process

createTableFlowTemplates :: MonadDB m => Migration m
createTableFlowTemplates = Migration
  { mgrTableName = "flow_templates"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_templates"
      , tblVersion     = 1
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
        , tblColumn { colName     = "created"
                    , colType     = TimestampWithZoneT
                    , colNullable = False
                    }
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
  }

createTableFlowInstances :: MonadDB m => Migration m
createTableFlowInstances = Migration
  { mgrTableName = "flow_instances"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_instances"
      , tblVersion     = 1
      , tblColumns     =
        [ tblColumn { colName     = "id"
                    , colType     = UuidT
                    , colNullable = False
                    , colDefault  = Just "gen_random_uuid()"
                    }
        , tblColumn { colName = "template_id", colType = UuidT, colNullable = False }
        , tblColumn { colName = "current_state", colType = TextT, colNullable = False }
        , tblColumn { colName     = "created"
                    , colType     = TimestampWithZoneT
                    , colNullable = False
                    }
        ]
      , tblPrimaryKey  = pkOnColumn "id"
      , tblIndexes     = [indexOnColumn "template_id"]
      , tblForeignKeys = [fkOnColumn "template_id" "flow_templates" "id"]
      }
  }

createTableFlowInstanceKeyValueStore :: MonadDB m => Migration m
createTableFlowInstanceKeyValueStore = Migration
  { mgrTableName = "flow_instance_key_value_store"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_instance_key_value_store"
      , tblVersion     = 1
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
                         ]
      , tblForeignKeys =
        [ (fkOnColumn "instance_id" "flow_instances" "id") { fkOnDelete = ForeignKeyCascade
                                                           }
        , fkOnColumn "document_id" "documents" "id"
        , fkOnColumn "user_id" "users" "id"
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
  }

createTableFlowUserAuthConfigs :: MonadDB m => Migration m
createTableFlowUserAuthConfigs = Migration
  { mgrTableName = "flow_user_auth_configs"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_user_auth_configs"
      , tblVersion     = 1
      , tblColumns     =
        [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
        , tblColumn { colName = "key", colType = TextT, colNullable = False }
        , tblColumn { colName     = "auth_to_view_provider"
                    , colType     = TextT
                    , colNullable = True
                    }
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
  }

createTableFlowInstanceSignatories :: MonadDB m => Migration m
createTableFlowInstanceSignatories = Migration
  { mgrTableName = "flow_instance_signatories"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
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
  }

createTableFlowEvents :: MonadDB m => Migration m
createTableFlowEvents = Migration
  { mgrTableName = "flow_events"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_events"
      , tblVersion     = 1
      , tblColumns     =
        [ tblColumn { colName     = "id"
                    , colType     = UuidT
                    , colNullable = False
                    , colDefault  = Just "gen_random_uuid()"
                    }
        , tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
        , tblColumn { colName = "user_name", colType = TextT, colNullable = False }
        , tblColumn { colName = "document_name", colType = TextT, colNullable = False }
        , tblColumn { colName = "user_action", colType = TextT, colNullable = False }
        , tblColumn { colName     = "created"
                    , colType     = TimestampWithZoneT
                    , colNullable = False
                    }
        ]
      , tblPrimaryKey  = pkOnColumn "id"
      , tblIndexes     = [indexOnColumn "instance_id"]
      , tblForeignKeys =
        [ (fkOnColumn "instance_id" "flow_instances" "id") { fkOnDelete = ForeignKeyCascade
                                                           }
        ]
      }
  }

createTableFlowAggregatorEvents :: MonadDB m => Migration m
createTableFlowAggregatorEvents = Migration
  { mgrTableName = "flow_aggregator_events"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_aggregator_events"
      , tblVersion     = 1
      , tblColumns = [tblColumn { colName = "id", colType = UuidT, colNullable = False }]
      , tblPrimaryKey  = pkOnColumn "id"
      , tblForeignKeys =
        [(fkOnColumn "id" "flow_events" "id") { fkOnDelete = ForeignKeyCascade }]
      }
  }

createTableFlowInstanceAccessTokens :: MonadDB m => Migration m
createTableFlowInstanceAccessTokens = Migration
  { mgrTableName = "flow_instance_access_tokens"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName        = "flow_instance_access_tokens"
      , tblVersion     = 1
      , tblColumns     =
        [ tblColumn { colName     = "id"
                    , colType     = UuidT
                    , colNullable = False
                    , colDefault  = Just "gen_random_uuid()"
                    }
      -- TODO: add expiration time?
        , tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
        , tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
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
  }

createTableFlowInstanceSessions :: MonadDB m => Migration m
createTableFlowInstanceSessions = Migration
  { mgrTableName = "flow_instance_sessions"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
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
  }

addIndicesToFlowInstanceKeyValueStore :: MonadDB m => Migration m
addIndicesToFlowInstanceKeyValueStore = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlCreateIndexSequentially tableName $ indexOnColumn
                       "string"
                     runQuery_ . sqlCreateIndexSequentially tableName $ indexOnColumn
                       "user_id"
  }
  where tableName = "flow_instance_key_value_store"

addMetaDataToInstanceTable :: MonadDB m => Migration m
addMetaDataToInstanceTable = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable tableName ["RENAME COLUMN created TO started"]
      runQuery_ $ sqlAlterTable
        tableName
        [ sqlAddColumn
          $ tblColumn { colName = "title", colType = TextT, colNullable = True }
        , sqlAddColumn $ tblColumn { colName     = "last_event"
                                   , colType     = TimestampWithZoneT
                                   , colNullable = False
                                   , colDefault  = Just "now()"
                                   }
        ]
      runQuery_ $ sqlAlterTable tableName [sqlAlterColumn "last_event" "DROP DEFAULT"]
  }
  where tableName = "flow_instances"

createTableCallbacks :: MonadDB m => Migration m
createTableCallbacks = Migration
  { mgrTableName = tableName
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration . createTable True $ tblTable
      { tblName       = tableName
      , tblVersion    = 1
      , tblColumns = [ tblColumn { colName     = "id"
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
  }
  where tableName = "flow_callbacks"

addCallbacksToInstanceTable :: MonadDB m => Migration m
addCallbacksToInstanceTable = Migration
  { mgrTableName = tableName
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       tableName
                       [ sqlAddColumn $ tblColumn { colName     = "callback_id"
                                                  , colType     = UuidT
                                                  , colNullable = True
                                                  }
                       , sqlAddValidFK tableName
                         $ fkOnColumn "callback_id" "flow_callbacks" "id"
                       ]
  }
  where tableName = "flow_instances"

migrateTemplateDSLStoredInDBToNotificationMethods
  :: (MonadDB m, MonadLog m) => Migration m
migrateTemplateDSLStoredInDBToNotificationMethods = Migration
  { mgrTableName = "flow_templates"
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlSelect "flow_templates" $ do
                       sqlResult "id"
                       sqlResult "process"
                       sqlWhereIsNotNULL "committed"
                     templates <- fetchMany identity
                     forM_ templates $ \(tid :: TemplateId, process) -> do
                       case updateYAML <$> decodeProcess process of
                         Right (Just value) -> do

                           let process' = encode value

                           logInfo "parsed to Value" value
                           logInfo "extruding new process" $ TE.decodeUtf8 process'

                           runQuery_ . sqlUpdate "flow_templates" $ do
                             sqlSet "process" $ TE.decodeUtf8 process'
                             sqlWhereEq "id" tid
                         _ -> pure () -- ignore for now
  }
  where
    decodeProcess :: Process -> Either ParseException Value
    decodeProcess = decodeEither' . TE.encodeUtf8 . fromProcess

    updateYAML :: Value -> Maybe Value
    updateYAML value = updateNotifications value >>= setDSLVersion

    setDSLVersion :: Value -> Maybe Value
    setDSLVersion value = case value of
      Object o -> pure . Object $ HM.insert "dsl-version" "0.2.0" o
      _        -> Nothing

    updateNotifications :: Value -> Maybe Value
    updateNotifications value = case value of
      Object o -> case HM.lookup "stages" o of
        Nothing     -> Nothing
        Just stages -> updateStages stages >>= update o
      _ -> Nothing
      where update o stages = pure . Object $ HM.insert "stages" stages o

    updateStages :: Value -> Maybe Value
    updateStages = \case
      Array a -> fmap Array . sequence $ updateStage <$> a
      _       -> Nothing

    updateStage :: Value -> Maybe Value
    updateStage = \case
      Object o -> case HM.toList o of
        [(k, value)] -> updateStageInner value >>= update o k
        _            -> Nothing
      _ -> Nothing
      where update o k stages = pure . Object $ HM.insert k stages o

    updateStageInner :: Value -> Maybe Value
    updateStageInner value = case value of
      Object o -> case HM.lookup "actions" o of
        Nothing      -> pure value -- Actions are not mandatory => NOOP
        Just actions -> updateActions actions >>= update o
      _ -> Nothing
      where update o actions = pure . Object $ HM.insert "actions" actions o

    updateActions :: Value -> Maybe Value
    updateActions = \case
      Array a -> fmap Array . sequence $ updateAction <$> a
      _       -> Nothing

    updateAction :: Value -> Maybe Value
    updateAction value = case value of
      Object o -> case HM.lookup "notify" o of
        Nothing     -> pure value -- Actions are not mandatory => NOOP
        Just action -> updateNotify action >>= update o
      _ -> Nothing
      where update o action = pure . Object $ HM.insert "notify" action o

    updateNotify :: Value -> Maybe Value
    updateNotify value = case value of
      Object o -> case HM.lookup "message" o of
        Just (String msg) ->
          pure . Object . HM.insert "methods" (makeMethods msg) $ HM.delete "message" o
        _ -> Nothing
      _ -> pure value -- May already be a 0.2.0 DSL => NOOP

    makeMethods :: Text -> Value
    makeMethods msg = Object $ HM.fromList [("sms", String msg), ("email", String msg)]
