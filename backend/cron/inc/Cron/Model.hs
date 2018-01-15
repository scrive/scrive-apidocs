module Cron.Model (cronConsumer) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG (CryptoRNG)
import Data.Int
import Data.Time (diffUTCTime)
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes
import Log
import Network.HTTP.Client (Manager)
import qualified Data.Text as T

import Administration.Invoicing
import Amazon (AmazonMonad)
import CronConf
import DB
import Doc.Action
import Doc.AutomaticReminder.Model (expireDocumentAutomaticReminders)
import Doc.Model
import HostClock.Collector (collectClockError)
import KontraPrelude
import Log.Configuration
import Log.Identifier
import Log.Model
import Log.Utils
import Mails.Events
import MinutesTime
import Planhat
import Purging.Files
import Session.Model (DeleteExpiredSessions(..))
import SMS.Events
import ThirdPartyStats.Core
import User.Data.User (User(..))
import User.EmailChangeRequest (DeleteExpiredEmailChangeRequests(..))
import User.Model.Query (GetUserWherePasswordAlgorithmIsEarlierThan(..))
import User.Model.Update (SetUserPassword(..))
import User.Password (PasswordAlgorithm(..), strengthenPassword)
import User.PasswordReminder (DeleteExpiredPasswordReminders(..))
import User.UserAccountRequest (expireUserAccountRequests)
import Utils.List
import qualified Amazon as AWS
import qualified CronEnv

data JobType
  = AmazonUpload
  | AsyncEventsProcessing
  | ClockErrorCollection
  | DocumentAutomaticRemindersEvaluation
  | DocumentSearchUpdate
  | DocumentsAuthorIDMigration
  | DocumentsPurge
  | DocumentsArchiveIdle
  | EmailChangeRequestsEvaluation
  | FindAndTimeoutDocuments
  | InvoicingUpload
  | MailEventsProcessing
  | MarkOrphanFilesForPurge
  | OldDraftsRemoval
  | OldLogsRemoval
  | PasswordRemindersEvaluation
  | PurgeOrphanFile
  | PushPlanhatStats
  | SessionsEvaluation
  | SMSEventsProcessing
  | StrengthenPasswords
  | UserAccountRequestEvaluation
  deriving (Eq, Ord, Show)

jobTypeMapper :: [(JobType, T.Text)]
jobTypeMapper = [
    (AmazonUpload, "amazon_upload")
  , (AsyncEventsProcessing, "async_events_processing")
  , (ClockErrorCollection, "clock_error_collection")
  , (DocumentAutomaticRemindersEvaluation, "document_automatic_reminders_evaluation")
  , (DocumentsPurge, "documents_purge")
  , (DocumentsArchiveIdle, "documents_archive_idle")
  , (EmailChangeRequestsEvaluation, "email_change_requests_evaluation")
  , (FindAndTimeoutDocuments, "find_and_timeout_documents")
  , (InvoicingUpload, "invoice_upload")
  , (MailEventsProcessing, "mail_events_processing")
  , (MarkOrphanFilesForPurge, "mark_orphan_files_for_purge")
  , (OldDraftsRemoval, "old_drafts_removal")
  , (OldLogsRemoval, "old_logs_removal")
  , (PasswordRemindersEvaluation, "password_reminders_evaluation")
  , (PurgeOrphanFile, "purge_orphan_file")
  , (PushPlanhatStats, "push_planhat_stats")
  , (SessionsEvaluation, "sessions_evaluation")
  , (SMSEventsProcessing, "sms_events_processing")
  , (StrengthenPasswords, "upgrade_password_algorithm")
  , (UserAccountRequestEvaluation, "user_account_request_evaluation")
  , (DocumentSearchUpdate, "document_search_update")
  , (DocumentsAuthorIDMigration, "document_author_id_job")
  ]

instance PQFormat JobType where
  pqFormat = const $ pqFormat (undefined::T.Text)

instance FromSQL JobType where
  type PQBase JobType = PQBase T.Text
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing -> throwM InvalidValue {
        ivValue = v
      , ivValidValues = Just $ map snd jobTypeMapper
      }

instance ToSQL JobType where
  type PQDest JobType = PQBase T.Text
  toSQL tt = toSQL . fromJust $ tt `lookup` jobTypeMapper

----------------------------------------

data CronJob = CronJob {
  cjType      :: !JobType
, cjAttempts  :: !Int32
} deriving (Eq, Ord, Show)

cronJobSelectors :: [SQL]
cronJobSelectors = ["id", "attempts"]

cronJobFetcher :: (JobType, Int32) -> CronJob
cronJobFetcher (jtype, attempts) = CronJob {
  cjType = jtype
, cjAttempts = attempts
}

---------------------------------------

cronConsumer
  :: ( CryptoRNG m, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadIO m
     , MonadLog m, MonadMask m, MonadThrow m, MonadTime m
     , AmazonMonad cronenv, CryptoRNG cronenv, MonadBaseControl IO cronenv
     , MonadDB cronenv, MonadIO cronenv, MonadLog cronenv, MonadMask cronenv
     , MonadReader CronEnv.CronEnv cronenv
     , CryptoRNG dbt, MonadBaseControl IO dbt, MonadCatch dbt, MonadDB dbt
     , MonadIO dbt, MonadLog dbt, MonadThrow dbt, MonadTime dbt)
  => CronConf
  -> Manager
  -> Maybe (EventProcessor dbt)
  -> Maybe (EventProcessor dbt)
  -> (forall r. cronenv r -> m r)
  -> (forall r. dbt r -> m r)
  -> Int
  -> ConsumerConfig m JobType CronJob
cronConsumer cronConf mgr mmixpanel mplanhat runCronEnv runDB maxRunningJobs = ConsumerConfig {
  ccJobsTable = "cron_jobs"
, ccConsumersTable = "cron_workers"
, ccJobSelectors = cronJobSelectors
, ccJobFetcher = cronJobFetcher
, ccJobIndex = cjType
, ccNotificationChannel = Nothing
, ccNotificationTimeout = 3 * 1000000
, ccMaxRunningJobs = maxRunningJobs
, ccProcessJob = \CronJob{..} -> logHandlerInfo cjType $ do
  logInfo_ "Processing job"
  action <- case cjType of
    AmazonUpload -> do
      if AWS.isAWSConfigOk $ cronAmazonConfig cronConf
        then do
          moved <- runCronEnv (AWS.uploadSomeFilesToAmazon 10)
          if moved
            then return . RerunAfter $ iseconds 1
            else return . RerunAfter $ iminutes 1
        else do
          logInfo_ "No valid AWS config, skipping"
          return . RerunAfter $ iminutes 1
    AsyncEventsProcessing -> do
      runDB $ do
        let processMaximum = 200
        asyncProcessEvents getEventProcessor (NoMoreThan processMaximum)
      return . RerunAfter $ iseconds 10
    ClockErrorCollection -> do
      runDB $ collectClockError (cronNtpServers cronConf)
      return . RerunAfter $ ihours 1
    DocumentAutomaticRemindersEvaluation -> do
      runCronEnv expireDocumentAutomaticReminders
      return . RerunAfter $ iminutes 1
    DocumentsAuthorIDMigration -> do
      let batchSize = 1000
      runDB $ do
        startTime <- currentTime
        ress <- dbUpdate $ UpdateAuthorUserID batchSize
        endTime <- currentTime
        let delta = diffUTCTime endTime startTime
        logInfo "Document author user ID updated" $ object
                [ "items_updated" .= ress
                , "elapsed_time" .= (realToFrac delta :: Double) ]
      now <- currentTime
      if now < todayAtHour 4 now
      then RerunAfter <$> return (iseconds 2)
      else RerunAt . nextDayMidnight <$> currentTime
    DocumentsPurge -> do
      runDB $ do
        startTime <- currentTime
        purgedCount <- dbUpdate . PurgeDocuments 30 $ fromIntegral unsavedDocumentLingerDays
        finishTime <- currentTime
        logInfo "Purged documents" $ object [
            "purged" .= purgedCount
          , "elapsed_time" .= (realToFrac (diffUTCTime finishTime startTime) :: Double)
          ]
      return . RerunAfter $ iminutes 10
    DocumentsArchiveIdle -> do
      now <- currentTime
      archived <- runDB $ archiveIdleDocuments now
      logInfo "DocumentsArchiveIdle finished" $ object [
          "total_documents_archived" .= archived
        ]
      RerunAt . nextDayAtHour 19 <$> currentTime
    DocumentSearchUpdate -> do
      runDB updateHistoricalSearchData
      now <- currentTime
      if now < todayAtHour 4 now
      then RerunAfter <$> return (iseconds 1)
      else RerunAt . nextDayMidnight <$> currentTime
    EmailChangeRequestsEvaluation -> do
      runDB . dbUpdate $ DeleteExpiredEmailChangeRequests
      return . RerunAfter $ ihours 1
    FindAndTimeoutDocuments -> do
      runCronEnv findAndTimeoutDocuments
      return . RerunAfter $ iminutes 10
    InvoicingUpload -> do
      case cronInvoicingSFTPConf cronConf of
        Nothing -> do
          logInfo "SFTP config missing; skipping" $ object []
        Just sftpConfig -> runDB $ uploadInvoicing sftpConfig
      RerunAt . nextDayAtHour 1 <$> currentTime
    MailEventsProcessing -> do
      runCronEnv Mails.Events.processEvents
      return . RerunAfter $ iseconds 5
    MarkOrphanFilesForPurge -> do
      let maxMarked = 100000
          -- Share the string between all the log messages.
          orphanFileMarked = "Orphan file marked for purge"
      fids <- runDB . dbUpdate . MarkOrphanFilesForPurgeAfter maxMarked $ idays 7
      forM_ fids $ \fid -> logInfo orphanFileMarked $ object [identifier_ fid]
      -- If maximum amount of files was marked, run it again shortly after.
      if length fids == maxMarked
        then return . RerunAfter $ iseconds 1
        else RerunAt . nextDayMidnight <$> currentTime
    OldDraftsRemoval -> do
      runDB $ do
        delCount <- dbUpdate $ RemoveOldDrafts 100
        logInfo "Removed old, unsaved draft documents" $ object [
            "removed" .= delCount
          ]
      return . RerunAfter $ ihours 1
    OldLogsRemoval -> do
      let connSource ci = simpleSource def { csConnInfo = ci }
          logDBs = catMaybes . for (lcLoggers $ cronLogConfig cronConf) $ \case
            PostgreSQL ci -> Just ci
            _             -> Nothing
      forM_ logDBs $ \ci -> runDBT (unConnectionSource $ connSource ci) def $ do
        runSQL_ "SELECT current_database()::text"
        dbName :: T.Text <- fetchOne runIdentity
        n <- dbUpdate $ CleanLogsOlderThanDays 30
        logInfo "Old logs removed" $ object [
            "database" .= dbName
          , "logs_removed" .= n
          ]
      RerunAt . nextDayMidnight <$> currentTime
    PasswordRemindersEvaluation -> do
      runDB . dbUpdate $ DeleteExpiredPasswordReminders
      return . RerunAfter $ ihours 1
    PurgeOrphanFile -> do
      found <- runCronEnv purgeOrphanFile
      return . RerunAfter $ if found
                            then iseconds 1
                            else iminutes 1
    PushPlanhatStats -> do
      case cronPlanhatConf cronConf of
        Nothing -> do
          logInfo "Planhat config missing; skipping" $ object []
        Just phConf -> do runDB $ doDailyPlanhatStats phConf mgr
      RerunAt . nextDayAtHour 2 <$> currentTime
    SessionsEvaluation -> do
      runDB . dbUpdate $ DeleteExpiredSessions
      return . RerunAfter $ ihours 1
    StrengthenPasswords -> do
      runDB $ do
        muser <- dbQuery $ GetUserWherePasswordAlgorithmIsEarlierThan
                           PasswordAlgorithmScrypt
        case muser of
          -- If no passwords to strengthen, we can relax for a while
          Nothing   -> return . RerunAfter $ idays 14
          Just user ->
            case userpassword user of
              Nothing -> do
                logAttention
                  ( "StrengthenPasswords found a user without a password"
                    <> ", should not happen" ) $ object [
                              "user_id" .= (show . userid $ user)
                              ]
                return . RerunAfter $ iseconds 1
              Just password -> do
                newPassword <- strengthenPassword password
                void $ dbUpdate $ SetUserPassword (userid user) newPassword
                return . RerunAfter $ iseconds 5
    SMSEventsProcessing -> do
      runCronEnv $ SMS.Events.processEvents
      return . RerunAfter $ iseconds 5
    UserAccountRequestEvaluation -> do
      runDB expireUserAccountRequests
      return . RerunAfter $ ihours 1
  logInfo_ "Job processed successfully"
  return $ Ok action
, ccOnException = \_ CronJob{..} -> return $ case cjAttempts of
  1 -> RerunAfter $ iminutes 1
  2 -> RerunAfter $ iminutes 5
  3 -> RerunAfter $ iminutes 10
  4 -> RerunAfter $ iminutes 15
  5 -> RerunAfter $ iminutes 30
  _ -> RerunAfter $ ihours 1
}
  where
    logHandlerInfo jobType action = localRandomID "job_id" $ localData ["job_type" .= show jobType] action

    getEventProcessor EventMixpanel = mmixpanel
    getEventProcessor EventPlanhat  = mplanhat
