module Doc.Extending.Migrations
  ( createDocumentExtendingJobs
  , createDocumentExtendingConsumers
  , documentExtendingJobsAddStatsIndexes
  ) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import Doc.Extending.Tables

createDocumentExtendingJobs :: MonadDB m => Migration m
createDocumentExtendingJobs = Migration
  { mgrTableName = tblName tableDocumentExtendingJobs
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "document_extending_jobs"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "id", colType = BigIntT, colNullable = False }
          , tblColumn { colName     = "run_at"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
          , tblColumn { colName = "reserved_by", colType = BigIntT }
          , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
          ]
        , tblPrimaryKey  = pkOnColumn "id"
        , tblForeignKeys =
          [ (fkOnColumn "id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
          , (fkOnColumn "reserved_by" "document_extending_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                                           }
          ]
        }
  }

createDocumentExtendingConsumers :: MonadDB m => Migration m
createDocumentExtendingConsumers = Migration
  { mgrTableName = tblName tableDocumentExtendingConsumers
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName       = "document_extending_consumers"
        , tblVersion    = 1
        , tblColumns    =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName = "name", colType = TextT, colNullable = False }
          , tblColumn { colName     = "last_activity"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          ]
        , tblPrimaryKey = pkOnColumn "id"
        }
  }

documentExtendingJobsAddStatsIndexes :: MonadDB m => Migration m
documentExtendingJobsAddStatsIndexes =
  let tname = tblName tableDocumentExtendingJobs
  in  Migration
        { mgrTableName = tname
        , mgrFrom      = 1
        , mgrAction    =
          StandardMigration $ do
            runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "run_at"
            runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "attempts")
              { idxWhere = Just "attempts > 1 AND finished_at IS NULL"
              }
        }
