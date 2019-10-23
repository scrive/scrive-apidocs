module Doc.Signing.Migrations where

import DB
import Doc.Signing.Tables

documentSigningJobsUseJson :: MonadDB m => Migration m
documentSigningJobsUseJson = Migration
  { mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runSQL_
        "ALTER TABLE document_signing_jobs ALTER COLUMN fields TYPE json USING fields::json"
      runSQL_
        "ALTER TABLE document_signing_jobs ALTER COLUMN screenshots TYPE json USING screenshots::json"
  }

documentSigningJobsAddSignatoryAttachments :: MonadDB m => Migration m
documentSigningJobsAddSignatoryAttachments = Migration
  { mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "document_signing_jobs"
                       [ sqlAddColumn tblColumn { colName = "not_uploaded_sig_attachments"
                                                , colType     = ArrayT TextT
                                                , colNullable = False
                                                , colDefault  = Just "'{}'::text[]"
                                                }
                       ]
  }

documentSigningJobsAddSignatureProvider :: MonadDB m => Migration m
documentSigningJobsAddSignatureProvider = Migration
  { mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom      = 3
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "document_signing_jobs"
                       [ sqlAddColumn tblColumn { colName     = "signature_provider"
                                                , colType     = SmallIntT
                                                , colNullable = False
                                                , colDefault  = Just "5"
                                                } -- CgiGrpBankID
                       ]
  }

documentSigningJobsAddConsentResponses :: MonadDB m => Migration m
documentSigningJobsAddConsentResponses = Migration
  { mgrTableName = tblName tableDocumentSigningJobs
  , mgrFrom      = 4
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "document_signing_jobs"
                       [ sqlAddColumn tblColumn { colName     = "consent_responses"
                                                , colType     = JsonT
                                                , colNullable = False
                                                , colDefault  = Just "'[]'::json"
                                                }
                       ]
  }
