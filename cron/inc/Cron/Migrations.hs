module Cron.Migrations (cronMigrations) where

import Data.ByteString (ByteString)

import Cron.Tables
import DB
import DB.Checks

cronMigrations :: MonadDB m => [Migration m]
cronMigrations = [
    createCronWorkersTable
  , createCronTasksTable
  , addArchiveIdleDocumentsTask
  , changeAPICallbacksExecutionFrequency
  , changeFindAndExtendDigitalSignaturesFrequency
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

createCronTasksTable :: MonadDB m => Migration m
createCronTasksTable = Migration {
  mgrTable = tableCronTasks
, mgrFrom = 0
, mgrDo = createTable tblTable {
    tblName = "cron_tasks"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "type",      colType = TextT, colNullable = False }
    , tblColumn { colName = "frequency", colType = IntervalT, colNullable = False }
    , tblColumn { colName = "started",   colType = TimestampWithZoneT }
    , tblColumn { colName = "finished",  colType = TimestampWithZoneT }
    , tblColumn { colName = "worker_id", colType = BigIntT }
    ]
  , tblPrimaryKey = pkOnColumn "type"
  , tblForeignKeys = [
      (fkOnColumn "worker_id" "cron_workers" "id") {
        fkOnDelete = ForeignKeySetNull
      }
    ]
  , tblInitialData = Just $ Rows ["type", "frequency"] ([
      ("amazon_deletion", ihours 3)
    , ("amazon_upload", iminutes 1)
    , ("async_events_processing", iseconds 10)
    , ("clock_error_collection", ihours 1)
    , ("document_api_callback_evaluation", iseconds 10)
    , ("document_automatic_reminders_evaluation", iminutes 1)
    , ("documents_purge", iminutes 10)
    , ("email_change_requests_evaluation", ihours 1)
    , ("find_and_do_post_document_closed_actions", ihours 6)
    , ("find_and_do_post_document_closed_actions_new", iminutes 10)
    , ("find_and_extend_digital_signatures", ihours 3)
    , ("find_and_timeout_documents", iminutes 10)
    , ("mail_events_processing", iseconds 5)
    , ("old_drafts_removal", ihours 1)
    , ("password_reminders_evaluation", ihours 1)
    , ("recurly_synchronization", iminutes 55)
    , ("sessions_evaluation", ihours 1)
    , ("sms_events_processing", iseconds 5)
    , ("user_account_request_evaluation", ihours 1)
    ] :: [(ByteString, Interval)])
  }
}

addArchiveIdleDocumentsTask :: MonadDB m => Migration m
addArchiveIdleDocumentsTask = Migration {
  mgrTable = tableCronTasks
, mgrFrom = 1
, mgrDo = do
    runQuery_ $ sqlInsert "cron_tasks" $ do
      sqlSet "type" ("documents_archive_idle"::String)
      sqlSet "frequency" (ihours 6)
}

changeAPICallbacksExecutionFrequency :: MonadDB m => Migration m
changeAPICallbacksExecutionFrequency = Migration {
  mgrTable = tableCronTasks
, mgrFrom = 2
, mgrDo = runQuery_ $ sqlUpdate "cron_tasks" $ do
    sqlSet "frequency" (iseconds 2)
    sqlWhereEq "type" ("document_api_callback_evaluation"::String)
}

changeFindAndExtendDigitalSignaturesFrequency :: MonadDB m => Migration m
changeFindAndExtendDigitalSignaturesFrequency = Migration {
  mgrTable = tableCronTasks
, mgrFrom = 3
, mgrDo = runQuery_ $ sqlUpdate "cron_tasks" $ do
    sqlSet "frequency" (iminutes 30)
    sqlWhereEq "type" ("find_and_extend_digital_signatures"::String)
}
