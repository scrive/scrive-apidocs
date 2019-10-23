{- |

To add a new cron task, the following steps are necessary:

1. Add a row corresponding to the task to Cron.Tables.tableCronTasks.tblInitialSetup.
2. Write a migration that adds the same row to the existing table.
3. Add a type constructor corresponding to the task to Cron.Model.JobType.
4. Update Cron.Model.jobTypeMapper.
5. Update dispatcher function in the main cron module.

-}
module Cron.Tables (
    cronTables
  , tableCronWorkers
  , tableCronJobs
  ) where

import Data.Text (Text)

import DB

cronTables :: [Table]
cronTables = [tableCronWorkers, tableCronJobs]

-- | Contains the list of currently running
-- (modulo ~1 minute) cron workers (instances).
tableCronWorkers :: Table
tableCronWorkers = tblTable
  { tblName       = "cron_workers"
  , tblVersion    = 2
  , tblColumns = [
    -- Id of the worker.
                   tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    -- Last activity of the worker.
                 , tblColumn { colName     = "last_activity"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
                 , tblColumn { colName = "name", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey = pkOnColumn "id"
  }

-- | The place for cron jobs.
tableCronJobs :: Table
tableCronJobs = tblTable
  { tblName         = "cron_jobs"
  , tblVersion      = 25
  , tblColumns = [
    -- Type of the task.
                   tblColumn { colName = "id", colType = TextT, colNullable = False }
    -- Time to run a task.
                 , tblColumn { colName     = "run_at"
                             , colType     = TimestampWithZoneT
                             , colNullable = False
                             }
    -- Time of the last finish (NULL if last run failed).
                 , tblColumn { colName = "finished_at", colType = TimestampWithZoneT }
                 , tblColumn { colName = "reserved_by", colType = BigIntT }
                 , tblColumn { colName     = "attempts"
                             , colType     = IntegerT
                             , colNullable = False
                             , colDefault  = Just "0"
                             }
                 ]
  , tblPrimaryKey   = pkOnColumn "id"
  , tblForeignKeys  =
    [(fkOnColumn "reserved_by" "cron_workers" "id") { fkOnDelete = ForeignKeySetNull }]
  , tblInitialSetup =
    Just $ TableInitialSetup
      { checkInitialSetup = return True
      , initialSetup      = forM_ tasks $ \task -> do
                              runSQL_
                                $   "INSERT INTO cron_jobs (id, run_at) VALUES ("
                                <?> task
                                <>  ", to_timestamp(0))"
      }
  }
  where
    tasks :: [Text]
    tasks =
      [ "async_events_processing"
      , "clock_error_collection"
      , "document_automatic_reminders_evaluation"
      , "documents_archive_idle"
      , "documents_purge"
      , "email_change_requests_evaluation"
      , "find_and_timeout_documents"
      , "mail_events_processing"
      , "mark_orphan_files_for_purge"
      , "monthly_invoice"
      , "old_drafts_removal"
      , "old_logs_removal"
      , "password_reminders_evaluation"
      , "push_planhat_stats"
      , "sessions_evaluation"
      , "sms_events_processing"
      , "user_account_request_evaluation"
      , "document_search_update"
      , "document_author_id_job"
      , "timeouted_signatory_access_tokens_purge"
      , "temporary_login_tokens_purge"
      , "cron_stats"
      , "timeouted_eid_transactions_purge"
      ]
