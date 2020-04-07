module CronStats.Model
  ( CronStats(..)
  , GetCronStats(..)
  , JobsTableStats(..)
  , cronStatsToList
  , jobTableStatsToList
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Int
import qualified Data.Text as T

import DB

data JobsTableStats = JobsTableStats
  { approximateJobsCount :: !Int64
  , overdueJobsCount     :: !Int64
  , failedJobsCount      :: !Int64
  }

data CronStats = CronStats
  { sealingStats     :: !JobsTableStats
  , signingStats     :: !JobsTableStats
  , apiCallbackStats :: !JobsTableStats
  , extendingStats   :: !JobsTableStats
  , filePurgeStats   :: !JobsTableStats
  , mailsStats       :: !JobsTableStats
  , smsesStats       :: !JobsTableStats
  }

cronStatsToList :: CronStats -> [(T.Text, JobsTableStats)]
cronStatsToList CronStats {..} =
  [ ("sealing"      , sealingStats)
  , ("signing"      , signingStats)
  , ("api_callbacks", apiCallbackStats)
  , ("extending"    , extendingStats)
  , ("file_purge"   , filePurgeStats)
  , ("mails"        , mailsStats)
  , ("smses"        , smsesStats)
  ]

jobTableStatsToList :: (T.Text, JobsTableStats) -> [(T.Text, Int64)]
jobTableStatsToList (job, JobsTableStats {..}) =
  [ (job <> "_approximate_jobs", approximateJobsCount)
  , (job <> "_overdue_jobs"    , overdueJobsCount)
  , (job <> "_failed_jobs"     , failedJobsCount)
  ]

-- There are 3 types of tables containing jobs:
--
-- 1. Inserted jobs are to be processed IMMEDIATELY and once processed, removed
-- from the table IMMEDIATELY (sealing, signing, api callbacks)
--
-- 2. Inserted jobs are to be processed IN THE FUTURE and once processed,
-- removed from the table IMMEDIATELY (extending, file purging).
--
-- 3. Inserted jobs are to be processed IMMEDIATELY and once processed, removed
-- from the table IN THE FUTURE (mails, smses).
--
-- Apart from checking approximate counts of the job tables, we want to monitor
-- exact count of overdue (run_at < now()) and failed (attempts > 1 AND
-- finished_at IS NULL) jobs.
--
-- For type 1 tables we don't need indexes on these columns as the tables are
-- supposed to be almost empty and any deviation from the norm will be
-- considered an emergency.
--
-- Type 2/3 tables might be somewhat large, so we need two indexes on them:
--
-- 1. On run_at (for both statistics and processing jobs). Note: for type 3
-- tables this index should be partial (run_at IS NOT NULL).
--
-- 2. On attempts, partial (attempts > 1 AND finished_at IS NULL) for fast
-- counting of not yet processed jobs that failed.
--
data GetCronStats = GetCronStats
instance (MonadDB m, MonadThrow m) => DBQuery m GetCronStats CronStats where
  dbQuery GetCronStats = do
    sealingStats     <- fetchJobTableStats "document_sealing_jobs"
    signingStats     <- fetchJobTableStats "document_signing_jobs"
    apiCallbackStats <- fetchJobTableStats "document_api_callbacks"
    extendingStats   <- fetchJobTableStats "document_extending_jobs"
    filePurgeStats   <- fetchJobTableStats "file_purge_jobs"
    mailsStats       <- fetchJobTableStats "mails"
    smsesStats       <- fetchJobTableStats "smses"
    pure CronStats { .. }
    where
      fetchJobTableStats table = do
        approximateJobsCount <- fetchApproxCount $ T.pack table
        overdueJobsCount     <- fetchOverdueCount $ unsafeSQL table
        failedJobsCount      <- fetchFailedCount $ unsafeSQL table
        pure JobsTableStats { .. }

      fetchOverdueCount = fetchCount $ do
        sqlWhere "run_at < now()"
      fetchFailedCount = fetchCount $ do
        sqlWhere "attempts > 1"
        sqlWhere "finished_at IS NULL"

      fetchCount :: State SqlSelect () -> SQL -> m Int64
      fetchCount where_ table = do
        let tmpTable = parenthesize . toSQLCommand . sqlSelect table $ do
              sqlResult "TRUE"
              where_
              -- Limit count to 10000 to cap the amount of traversed rows.
              sqlLimit 10000
        runQuery_ . sqlSelect (tmpTable <+> "items") $ do
          sqlResult "count(*)"
        fetchOne runIdentity

      fetchApproxCount :: T.Text -> m Int64
      fetchApproxCount table = do
        runQuery_ . sqlSelect "pg_class" $ do
          sqlResult "reltuples::bigint as count"
          sqlWhereEq "relname" table
        fetchOne runIdentity
