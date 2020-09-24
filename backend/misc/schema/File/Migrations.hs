module File.Migrations
  ( createFilePurgeConsumers
  , createFilePurgeJobs
  , dropContentAndPurgeAtFromFiles
  , filePurgingJobsAddStatsIndexes
  , createFileComposite
  ) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import File.Tables

createFilePurgeConsumers :: MonadDB m => Migration m
createFilePurgeConsumers = Migration
  { mgrTableName = tblName tableFilePurgeConsumers
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName       = "file_purge_consumers"
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

createFilePurgeJobs :: MonadDB m => Migration m
createFilePurgeJobs = Migration
  { mgrTableName = tblName tableFilePurgeJobs
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ do
      createTable
        True
        tblTable
          { tblName        = "file_purge_jobs"
          , tblVersion     = 1
          , tblColumns     =
            [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
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
            [ (fkOnColumn "id" "files" "id") { fkOnDelete = ForeignKeyCascade }
            , (fkOnColumn "reserved_by" "file_purge_consumers" "id") { fkOnDelete = ForeignKeySetNull
                                                                     }
            ]
          }
      runSQL_
        "INSERT INTO file_purge_jobs (id, run_at, attempts) SELECT id, purge_at, 0\
        \ FROM files WHERE purge_at IS NOT NULL AND purged_time IS NULL"
  }

dropContentAndPurgeAtFromFiles :: MonadDB m => Migration m
dropContentAndPurgeAtFromFiles = Migration
  { mgrTableName = tblName tableFiles
  , mgrFrom      = 9
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "files"
                       [sqlDropColumn "content", sqlDropColumn "purge_at"]
  }

filePurgingJobsAddStatsIndexes :: MonadDB m => Migration m
filePurgingJobsAddStatsIndexes =
  let tname = tblName tableFilePurgeJobs
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

createFileComposite :: MonadDB m => Migration m
createFileComposite = Migration
  { mgrTableName = tblName tableFiles
  , mgrFrom      = 10
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlCreateComposite $ CompositeType
                       { ctName    = "file_c1"
                       , ctColumns =
                         [ CompositeColumn { ccName = "id", ccType = BigIntT }
                         , CompositeColumn { ccName = "name", ccType = TextT }
                         , CompositeColumn { ccName = "amazon_url", ccType = TextT }
                         , CompositeColumn { ccName = "size", ccType = IntegerT }
                         , CompositeColumn { ccName = "checksum", ccType = BinaryT }
                         , CompositeColumn { ccName = "aes_key", ccType = BinaryT }
                         , CompositeColumn { ccName = "aes_iv", ccType = BinaryT }
                         ]
                       }
  }
