module Doc.Signing.Migrations where

import DB.Checks

import DB
import Doc.Signing.Tables
import KontraPrelude

documentSigningJobsUseJson :: MonadDB m => Migration m
documentSigningJobsUseJson = Migration {
    mgrTable = tableDocumentSigningJobs
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE document_signing_jobs ALTER COLUMN fields TYPE json USING fields::json"
      runSQL_ "ALTER TABLE document_signing_jobs ALTER COLUMN screenshots TYPE json USING screenshots::json"
  }

createDocumentSigningConsumersTable :: MonadDB m => Migration m
createDocumentSigningConsumersTable = Migration {
    mgrTable = tableDocumentSigningConsumers
  , mgrFrom = 0
  , mgrDo = createTable True tblTable {
      tblName = "document_signing_consumers"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "name", colType = TextT, colNullable = False }
      , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
      ]
      , tblPrimaryKey = pkOnColumn "id"
    }
  }

createDocumentSigningJobsTable :: MonadDB m => Migration m
createDocumentSigningJobsTable = Migration {
    mgrTable = tableDocumentSigningJobs
  , mgrFrom = 0
  , mgrDo = createTable True tblTable {
      tblName = "document_signing_jobs"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
      , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
      , tblColumn { colName = "reserved_by", colType = BigIntT }
      , tblColumn { colName = "cancelled", colType = BoolT, colNullable = False, colDefault = Just "false"}
      , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False }
      , tblColumn { colName = "branded_domain_id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False  }
      , tblColumn { colName = "client_ip_v4", colType = IntegerT, colNullable = False  }
      , tblColumn { colName = "client_time", colType = TimestampWithZoneT }
      , tblColumn { colName = "client_name", colType = TextT }
      , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
      , tblColumn { colName = "fields", colType = TextT, colNullable = False }
      , tblColumn { colName = "accepted_attachments", colType = ArrayT BigIntT, colNullable = False }
      , tblColumn { colName = "screenshots", colType = TextT, colNullable = False }
      , tblColumn { colName = "last_check_status", colType = TextT }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblForeignKeys = [
        (fkOnColumn "id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
      , (fkOnColumn "reserved_by" "document_signing_consumers" "id") {
          fkOnDelete = ForeignKeySetNull
        }
      , (fkOnColumn "branded_domain_id" "branded_domains" "id") {
          fkOnDelete = ForeignKeyCascade
        }
      ]
    }
  }
