module CronStats.Control ( reportCronStats )where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Log

import CronStats.Model
import DB
import StatsD.Communication
import StatsD.Config

reportCronStats
  :: (MonadDB m, MonadLog m, MonadThrow m, MonadIO m, MonadBaseControl IO m)
  => Maybe StatsDConf
  -> m ()
reportCronStats mconf = do
  cs <- dbQuery $ GetCronStats
  case mconf of
    Just sconf -> do
      reportCronStatsToStatsD sconf cs
    _ -> return ()
  logCronStats cs

reportCronStatsToStatsD
  :: (MonadThrow m, MonadLog m, MonadIO m, MonadBaseControl IO m)
  => StatsDConf
  -> CronStats
  -> m ()
reportCronStatsToStatsD conf stats =
  sendstats conf "cron_stats"
    $ map (\(n, fv) -> (n, fromIntegral $ fv stats))
    $ [ ("sealing_jobs"          , sealingJobsCount)
      , ("signing_jobs"          , signingJobsCount)
      , ("extending_jobs"        , extendingJobsCount)
      , ("overdue_extending_jobs", overdueExtendingJobsCount)
      , ("mail_sendouts"         , mailSendoutsCount)
      , ("sms_sendouts"          , smsSendoutsCount)
      , ("file_purging_jobs"     , filePurgingJobsCount)
      , ("api_callbacks"         , apiCallbacksCount)
      , ("retried_api_callbacks" , retriedCallbacksCount)
      ]


logCronStats :: (MonadLog m) => CronStats -> m ()
logCronStats stat =
  logInfo "Cron jobs stats"
    $ object
    $ [ "sealing_jobs" .= sealingJobsCount stat
      , "signing_jobs" .= signingJobsCount stat
      , "extending_jobs" .= extendingJobsCount stat
      , "overdue_extending_jobs" .= overdueExtendingJobsCount stat
      , "mail_sendouts" .= mailSendoutsCount stat
      , "sms_sendouts" .= smsSendoutsCount stat
      , "file_purging_jobs" .= filePurgingJobsCount stat
      , "api_callbacks" .= apiCallbacksCount stat
      , "retried_api_callbacks" .= retriedCallbacksCount stat
      ]
