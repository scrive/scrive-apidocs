module Doc.Signing.Migrations where

import DB
import Doc.Signing.Tables
import KontraPrelude

documentSigningJobsUseJson :: MonadDB m => Migration m
documentSigningJobsUseJson = Migration {
    mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runSQL_ "ALTER TABLE document_signing_jobs ALTER COLUMN fields TYPE json USING fields::json"
      runSQL_ "ALTER TABLE document_signing_jobs ALTER COLUMN screenshots TYPE json USING screenshots::json"
  }

documentSigningJobsAddSignatoryAttachments :: MonadDB m => Migration m
documentSigningJobsAddSignatoryAttachments = Migration {
    mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "document_signing_jobs" [
        sqlAddColumn tblColumn { colName = "not_uploaded_sig_attachments", colType = ArrayT TextT, colNullable = False, colDefault = Just "'{}'::text[]" }
        ]
  }
