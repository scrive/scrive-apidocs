module CronStats.Control
  ( reportCronStats
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control
import Data.Aeson
import Log

import CronStats.Model
import DB
import StatsD.Communication
import StatsD.Config

reportCronStats
  :: (MonadDB m, MonadLog m, MonadThrow m, MonadBaseControl IO m)
  => Maybe StatsDConf
  -> m ()
reportCronStats mconf = do
  cs <- cronStatsToList <$> dbQuery GetCronStats
  forM_ cs $ \(job, JobsTableStats {..}) -> do
    let attentionLimit = 500
    when (overdueJobsCount >= attentionLimit) $ do
      logAttention "Safe overdue job limit exceeded"
        $ object ["overdue_job_type" .= job, "overdue_job_count" .= overdueJobsCount]
    when (failedJobsCount >= attentionLimit) $ do
      logAttention "Safe failed job limit exceeded"
        $ object ["failed_job_type" .= job, "failed_job_count" .= failedJobsCount]
  let stats = concatMap jobTableStatsToList cs
  forM_ mconf $ \sconf -> sendStats sconf "cron_stats" stats
  logInfo "Cron jobs stats" . object $ map (uncurry (.=)) stats
