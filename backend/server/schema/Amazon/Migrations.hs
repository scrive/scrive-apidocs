module Amazon.Migrations where

import Database.PostgreSQL.PQTypes.Checks

import Amazon.Tables
import DB

createAmazonUploadJobs :: MonadDB m => Migration m
createAmazonUploadJobs = Migration {
    mgrTableName = tblName tableAmazonUploadJobs
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "amazon_upload_jobs"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
      , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
      , tblColumn { colName = "reserved_by", colType = BigIntT }
      , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblForeignKeys = [
        (fkOnColumn "id" "files" "id") { fkOnDelete = ForeignKeyCascade }
      , (fkOnColumn "reserved_by" "amazon_upload_consumers" "id") {
          fkOnDelete = ForeignKeySetNull
        }
      ]
    }
  }

createAmazonUploadConsumers :: MonadDB m => Migration m
createAmazonUploadConsumers = Migration {
    mgrTableName = tblName tableAmazonUploadConsumers
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "amazon_upload_consumers"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "name", colType = TextT, colNullable = False }
      , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    }
  }
