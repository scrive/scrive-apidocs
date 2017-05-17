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
