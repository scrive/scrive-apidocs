{- |

To add a new cron task, the following steps are necessary:

1. Add a row corresponding to the task to Cron.Tables.tableCronTasks.tblInitialData.
2. Write a migration that adds the same row to the existing table.
3. Add a type constructor corresponding to the task to Cron.Model.TaskType
4. Update Cron.Model.taskTypeBSRelation.
5. Update dispatcher function in the main cron module.

-}
module Cron.Tables (
    cronTables
  , tableCronWorkers
  , tableCronTasks
  ) where

import Data.ByteString (ByteString)
import DB

cronTables :: [Table]
cronTables = [
    tableCronWorkers
  , tableCronTasks
  ]

-- | Contains the list of currently running
-- (modulo ~2 minutes) cron workers (instances).
tableCronWorkers :: Table
tableCronWorkers = tblTable {
    tblName = "cron_workers"
  , tblVersion = 1
  , tblColumns = [
    -- | Id of the worker.
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    -- | Last activity of the worker.
    , tblColumn { colName = "last_activity", colType = TimestampWithZoneT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  }

-- | The place for cron configuration, i.e. list of
-- tasks to be run along with their frequencies etc.
tableCronTasks :: Table
tableCronTasks = tblTable {
    tblName = "cron_tasks"
  , tblVersion = 1
  , tblColumns = [
    -- | Type of the task.
      tblColumn { colName = "type",      colType = TextT, colNullable = False }
    -- | Interval frequency, NULL if task is a one time run.
    , tblColumn { colName = "frequency", colType = IntervalT, colNullable = False }
    -- | The time task was started. if NULL, task was never started before.
    , tblColumn { colName = "started",   colType = TimestampWithZoneT }
    -- | The time task was finished. if NULL, task was never finished before.
    , tblColumn { colName = "finished",  colType = TimestampWithZoneT }
    -- | Id of the worker that currently runs the task, NULL if the task isn't running.
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
