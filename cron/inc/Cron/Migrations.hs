module Cron.Migrations (cronMigrations) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid

import Cron.Tables
import DB
import DB.Checks

cronMigrations :: MonadDB m => [Migration m]
cronMigrations = [
    createCronWorkersTable
  , createCronJobsTable
  ]

createCronWorkersTable :: MonadDB m => Migration m
createCronWorkersTable = Migration {
  mgrTable = tableCronWorkers
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "cron_workers"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }
}

createCronJobsTable :: MonadDB m => Migration m
createCronJobsTable = Migration {
  mgrTable = tableCronJobs
, mgrFrom = 0
, mgrDo = do
  createTable tblTable {
    tblName = "cron_jobs"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = TextT, colNullable = False }
    , tblColumn { colName = "run_at", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
    , tblColumn { colName = "reserved_by", colType = BigIntT }
    , tblColumn { colName = "attempts", colType = IntegerT, colNullable = False, colDefault = Just "0" }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "reserved_by" "cron_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  }
  forM_ tasks $ \task -> do
    runSQL_ $ "INSERT INTO cron_jobs (id, run_at) VALUES (" <?> task <> ", now())"
}
  where
    tasks :: [ByteString]
    tasks = [
        "amazon_deletion"
      , "amazon_upload"
      , "async_events_processing"
      , "clock_error_collection"
      , "document_automatic_reminders_evaluation"
      , "documents_purge"
      , "documents_archive_idle"
      , "email_change_requests_evaluation"
      , "find_and_do_post_document_closed_actions"
      , "find_and_do_post_document_closed_actions_new"
      , "find_and_extend_digital_signatures"
      , "find_and_timeout_documents"
      , "mail_events_processing"
      , "old_drafts_removal"
      , "password_reminders_evaluation"
      , "recurly_synchronization"
      , "sessions_evaluation"
      , "sms_events_processing"
      , "user_account_request_evaluation"
      ]
