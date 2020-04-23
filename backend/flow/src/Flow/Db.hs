module Flow.Db where

import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Model

flowTables :: [Table]
flowTables = [ tableFlowTemplates ]

tableFlowTemplates :: Table
tableFlowTemplates = tblTable
  { tblName        = "flow_templates"
  , tblVersion     = 1
  , tblColumns     =
    [ tblColumn { colName = "id", colType = UuidT, colNullable = False, colDefault = Just "uuid_generate_v4()"}
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "process", colType = TextT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "committed", colType = TimestampWithZoneT }
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    ]
  , tblPrimaryKey  = pkOnColumn "id"
  , tblIndexes     =  [ indexOnColumn "user_id"
                      , indexOnColumn "user_group_id"
                      ]
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
