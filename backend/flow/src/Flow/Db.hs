module Flow.Db where

import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Model

flowTables :: [Table]
flowTables =
  [ tableFlowTemplates
  , tableFlowInstances
  , tableFlowStateMachines
  , tableFlowInstancesKVStore
  , tableFlowInstanceSignatories
  ]

flowMigrations :: MonadDB m => [Migration m]
flowMigrations =
  [ createTableFlowTemplates
  , createTableFlowInstances
  , createTableFlowStateMachines
  , createTableFlowInstancesKVStore
  , createTableFlowInstanceSignatories
  ]

tableFlowTemplates :: Table
tableFlowTemplates = tblTable
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
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "committed", colType = TimestampWithZoneT }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumn "user_id", indexOnColumn "user_group_id"]
  , tblForeignKeys =
    [
    -- Do not allow to delete users or user groups that still contain templates.
      (fkOnColumn "user_id" "users" "id") { fkOnDelete = ForeignKeyRestrict }
    , (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyRestrict }
    ]
  }

createTableFlowTemplates :: MonadDB m => Migration m
createTableFlowTemplates = Migration
  { mgrTableName = tblName tableFlowTemplates
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowTemplates
  }

tableFlowInstances :: Table
tableFlowInstances = tblTable
  { tblName        = "flow_instances"
  , tblVersion     = 1
  , tblColumns     = [ tblColumn { colName     = "id"
                                 , colType     = UuidT
                                 , colNullable = False
                                 , colDefault  = Just "gen_random_uuid()"
                                 }
                     , tblColumn { colName     = "template_id"
                                 , colType     = UuidT
                                 , colNullable = False
                                 }
                     ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     = [indexOnColumn "template_id"]
  , tblForeignKeys =
    [(fkOnColumn "template_id" "flow_templates" "id") { fkOnDelete = ForeignKeyRestrict }]
  }

createTableFlowInstances :: MonadDB m => Migration m
createTableFlowInstances = Migration
  { mgrTableName = tblName tableFlowInstances
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstances
  }

tableFlowInstancesKVStore :: Table
tableFlowInstancesKVStore = tblTable
  { tblName        = "flow_instance_key_value_store"
  , tblVersion     = 1
  , tblColumns = [ tblColumn { colName     = "instance_id"
                             , colType     = UuidT
                             , colNullable = False
                             }
                 -- TODO: `type` should be an enum.
                 -- The possible values are as follows.
                 -- * user: `email`, `phone_number`, `user_id`.
                 -- * document: `document_id`
                 -- * message: `string`.
                 , tblColumn { colName = "key", colType = TextT, colNullable = False }
                 , tblColumn { colName = "type", colType = TextT, colNullable = False }
                 , tblColumn { colName = "value", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey  = pkOnColumns ["instance_id", "key"]
  , tblForeignKeys =
    [(fkOnColumn "instance_id" "flow_instances" "id") { fkOnDelete = ForeignKeyRestrict }]
  }

createTableFlowInstancesKVStore :: MonadDB m => Migration m
createTableFlowInstancesKVStore = Migration
  { mgrTableName = tblName tableFlowInstancesKVStore
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstancesKVStore
  }

tableFlowInstanceSignatories :: Table
tableFlowInstanceSignatories = tblTable
  { tblName        = "flow_instance_signatories"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "instance_id", colType = UuidT, colNullable = False }
    , tblColumn { colName = "key", colType = TextT, colNullable = False }
    , tblColumn { colName = "signatory_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["instance_id", "key"]
  , tblIndexes     = [indexOnColumns ["instance_id", "signatory_id"]]
  , tblForeignKeys =
    [ (fkOnColumns ["instance_id", "key"]
                   "flow_instance_key_value_store"
                   ["instance_id", "key"]
      ) { fkOnDelete = ForeignKeyRestrict
        }
    , (fkOnColumn "signatory_id" "signatory_links" "id") { fkOnDelete = ForeignKeyRestrict
                                                         }
    ]
  }

createTableFlowInstanceSignatories :: MonadDB m => Migration m
createTableFlowInstanceSignatories = Migration
  { mgrTableName = tblName tableFlowInstanceSignatories
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowInstanceSignatories
  }

tableFlowStateMachines :: Table
tableFlowStateMachines = tblTable
  { tblName        = "flow_compiled_state_machine"
  , tblVersion     = 1
  , tblColumns = [ tblColumn { colName     = "template_id"
                             , colType     = UuidT
                             , colNullable = False
                             }
    -- TODO: Maybe use JSON type instead of Text???
                 , tblColumn { colName = "data", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey  = pkOnColumn "template_id"
  , tblForeignKeys =
    [(fkOnColumn "template_id" "flow_templates" "id") { fkOnDelete = ForeignKeyRestrict }]
  }

createTableFlowStateMachines :: MonadDB m => Migration m
createTableFlowStateMachines = Migration
  { mgrTableName = tblName tableFlowStateMachines
  , mgrFrom      = 0
  , mgrAction    = StandardMigration $ createTable True tableFlowStateMachines
  }
