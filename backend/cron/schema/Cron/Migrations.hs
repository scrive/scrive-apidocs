module Cron.Migrations (
    removeRecurlySynchronizationFromCronJobs
  , removeFindAndDoPostDocumentClosedActions
  , addInvoicingJob
  , addPlanhatJob
  , addPasswordAlgorithmUpgradeJob
  , removeFindAndExtendDigitalSignaturesFromCronJobs
  , addDocumentSearchUpdateJob
  , addDocumentAuthorUserIDUpdateJob
  , addUserGroupMigrationJob
  , removeUserGroupMigrationJob
  , addAttachmentsPurgeJob
  , addTemporaryMagicHashesPurgeJob
  , removeAmazonUploadJob
  , removePurgeOrphanFileJob
  , addTemporaryLoginTokensPurgeJob
  , addMonthlyInvoiceJob
  , addCronStatsJob
  , removePasswordAlgorithmUpgradeJob
) where

import Control.Monad.Catch

import Cron.Tables
import DB

addUserGroupMigrationJob :: (MonadDB m, MonadThrow m) => Migration m
addUserGroupMigrationJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 12
  , mgrAction =
      StandardMigration $
        runSQL_ $
              "INSERT INTO cron_jobs (id, run_at) VALUES"
          <+> "('user_group_migration', to_timestamp(0))"
  }


addDocumentAuthorUserIDUpdateJob :: (MonadDB m, MonadThrow m) => Migration m
addDocumentAuthorUserIDUpdateJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 10
  , mgrAction =
      StandardMigration $
        runSQL_ $
              "INSERT INTO cron_jobs (id, run_at) VALUES"
          <+> "('document_author_id_job', to_timestamp(0))"
  }

addDocumentSearchUpdateJob :: (MonadDB m, MonadThrow m) => Migration m
addDocumentSearchUpdateJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 9
  , mgrAction = StandardMigration $ runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('document_search_update', to_timestamp(0))"
  }

addPlanhatJob :: (MonadDB m, MonadThrow m) => Migration m
addPlanhatJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 7
  , mgrAction = StandardMigration $ runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('push_planhat_stats', to_timestamp(0))"
  }

addInvoicingJob :: (MonadDB m, MonadThrow m) => Migration m
addInvoicingJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 6
  , mgrAction = StandardMigration $ runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('invoice_upload', to_timestamp(0))"
  }

addPasswordAlgorithmUpgradeJob :: (MonadDB m, MonadThrow m) => Migration m
addPasswordAlgorithmUpgradeJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 11
  , mgrAction = StandardMigration $ runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('upgrade_password_algorithm', to_timestamp(0))"
  }

removeFindAndDoPostDocumentClosedActions :: (MonadDB m, MonadThrow m) => Migration m
removeFindAndDoPostDocumentClosedActions = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 5
  , mgrAction = StandardMigration $ do
      n <- runSQL "DELETE FROM cron_jobs WHERE id = 'find_and_do_post_document_closed_actions'"
      when (n /= 1) $ do
        unexpectedError "Wrong amount of rows deleted"
  }

removeRecurlySynchronizationFromCronJobs :: (MonadDB m, MonadThrow m) => Migration m
removeRecurlySynchronizationFromCronJobs = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 4
  , mgrAction = StandardMigration $ do
      n <- runSQL "DELETE FROM cron_jobs WHERE id = 'recurly_synchronization'"
      when (n /= 1) $ do
        unexpectedError "Wrong amount of rows deleted"
  }

removeFindAndExtendDigitalSignaturesFromCronJobs :: (MonadDB m, MonadThrow m) => Migration m
removeFindAndExtendDigitalSignaturesFromCronJobs = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 8
  , mgrAction = StandardMigration $ do
      n <- runSQL "DELETE FROM cron_jobs WHERE id = 'find_and_extend_digital_signatures'"
      when (n /= 1) $ do
        unexpectedError "Wrong amount of rows deleted"
  }

removeUserGroupMigrationJob :: (MonadDB m, MonadThrow m) => Migration m
removeUserGroupMigrationJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 13
  , mgrAction =
      StandardMigration $ runSQL_ "DELETE FROM cron_jobs WHERE id = 'user_group_migration'"
  }

addAttachmentsPurgeJob :: (MonadDB m, MonadThrow m) => Migration m
addAttachmentsPurgeJob = Migration
  { mgrTableName = tblName tableCronJobs
  , mgrFrom = 14
  , mgrAction = StandardMigration $
      runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('attachments_purge', to_timestamp(0))"
  }

addTemporaryMagicHashesPurgeJob :: (MonadDB m, MonadThrow m) => Migration m
addTemporaryMagicHashesPurgeJob = Migration
  { mgrTableName = tblName tableCronJobs
  , mgrFrom = 15
  , mgrAction = StandardMigration $
      runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('temporary_magic_hashes_purge', to_timestamp(0))"
  }

removeAmazonUploadJob :: (MonadDB m, MonadThrow m) => Migration m
removeAmazonUploadJob = Migration
  { mgrTableName = tblName tableCronJobs
  , mgrFrom = 16
  , mgrAction = StandardMigration $
      runSQL_ "DELETE FROM cron_jobs WHERE id = 'amazon_upload'"
  }

removePurgeOrphanFileJob :: (MonadDB m, MonadThrow m) => Migration m
removePurgeOrphanFileJob = Migration
  { mgrTableName = tblName tableCronJobs
  , mgrFrom = 17
  , mgrAction = StandardMigration $
      runSQL_ "DELETE FROM cron_jobs WHERE id = 'purge_orphan_file'"
  }

addTemporaryLoginTokensPurgeJob :: (MonadDB m, MonadThrow m) => Migration m
addTemporaryLoginTokensPurgeJob = Migration
  { mgrTableName = tblName tableCronJobs
  , mgrFrom = 18
  , mgrAction = StandardMigration $
      runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('temporary_login_tokens_purge', to_timestamp(0))"
  }

addMonthlyInvoiceJob :: (MonadDB m, MonadThrow m) => Migration m
addMonthlyInvoiceJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 19
  , mgrAction = StandardMigration $
      runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('monthly_invoice', to_timestamp(0))"
  }

addCronStatsJob :: (MonadDB m, MonadThrow m) => Migration m
addCronStatsJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 20
  , mgrAction = StandardMigration $
      runSQL_ "INSERT INTO cron_jobs (id, run_at) VALUES ('cron_stats', to_timestamp(0))"
  }

removePasswordAlgorithmUpgradeJob :: (MonadDB m, MonadThrow m) => Migration m
removePasswordAlgorithmUpgradeJob = Migration {
    mgrTableName = tblName tableCronJobs
  , mgrFrom = 21
  , mgrAction = StandardMigration $ runSQL_ "DELETE FROM cron_jobs WHERE id = 'upgrade_password_algorithm'"
  }

