module Cron.Model (cronConsumer) where

import Control.Arrow ((&&&))
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

import Attachment.Model
import CronConf
import CronStats.Control
import DB
import Doc.Action
import Doc.AutomaticReminder.Model (expireDocumentAutomaticReminders)
import Doc.DocumentID
import Doc.Model
import EID.EIDService.Model
import File.Storage
import HostClock.Collector (collectClockError)
import Log.Configuration
import Log.Identifier
import Log.Model
import Log.Utils
import Mails.Events
import MinutesTime
import MonthlyInvoice.Send
import Planhat
import Purging.Files
import Session.Model
  ( DeleteExpiredSessions(..), PurgeExpiredTemporaryLoginTokens(..)
  )
import SMS.Events
import ThirdPartyStats.Core
import User.EmailChangeRequest (DeleteExpiredEmailChangeRequests(..))
import User.PasswordReminder (DeleteExpiredPasswordReminders(..))
import User.UserAccountRequest (expireUserAccountRequests)
import UserGroup.Model
import Utils.List
import qualified CronEnv

data JobType
  = AsyncEventsProcessing
  | ClockErrorCollection
  | DocumentAutomaticRemindersEvaluation
  | DocumentSearchUpdate
  | DocumentsAuthorIDMigration
  | DocumentsPurge
  | DocumentsArchiveIdle
  | EmailChangeRequestsEvaluation
  | FindAndTimeoutDocuments
  | MailEventsProcessing
  | MarkOrphanFilesForPurge
  | MonthlyInvoice
  | OldDraftsRemoval
  | OldLogsRemoval
  | PasswordRemindersEvaluation
  | PushPlanhatStats
  | SessionsEvaluation
  | SMSEventsProcessing
  | UserAccountRequestEvaluation
  | AttachmentsPurge
  | TimeoutedSignatoryAccessTokensPurge
  | TemporaryLoginTokensPurge
  | CronStats
  | TimeoutedEIDTransactionsPurge
  | PopulateDocumentAuthorDeleted
  | UserGroupGarbageCollection
  deriving (Eq, Ord, Show, Enum, Bounded)

jobTypeMapper :: [(JobType, Text)]
jobTypeMapper = map (identity &&& jobTypeToText) [minBound .. maxBound]
  where
    jobTypeToText = \case
      AsyncEventsProcessing         -> "async_events_processing"
      ClockErrorCollection          -> "clock_error_collection"
      DocumentAutomaticRemindersEvaluation -> "document_automatic_reminders_evaluation"
      DocumentsPurge                -> "documents_purge"
      DocumentsArchiveIdle          -> "documents_archive_idle"
      EmailChangeRequestsEvaluation -> "email_change_requests_evaluation"
      FindAndTimeoutDocuments       -> "find_and_timeout_documents"
      MailEventsProcessing          -> "mail_events_processing"
      MarkOrphanFilesForPurge       -> "mark_orphan_files_for_purge"
      MonthlyInvoice                -> "monthly_invoice"
      OldDraftsRemoval              -> "old_drafts_removal"
      OldLogsRemoval                -> "old_logs_removal"
      PasswordRemindersEvaluation   -> "password_reminders_evaluation"
      PushPlanhatStats              -> "push_planhat_stats"
      SessionsEvaluation            -> "sessions_evaluation"
      SMSEventsProcessing           -> "sms_events_processing"
      UserAccountRequestEvaluation  -> "user_account_request_evaluation"
      DocumentSearchUpdate          -> "document_search_update"
      DocumentsAuthorIDMigration    -> "document_author_id_job"
      AttachmentsPurge              -> "attachments_purge"
      TimeoutedSignatoryAccessTokensPurge -> "timeouted_signatory_access_tokens_purge"
      TemporaryLoginTokensPurge     -> "temporary_login_tokens_purge"
      CronStats                     -> "cron_stats"
      TimeoutedEIDTransactionsPurge -> "timeouted_eid_transactions_purge"
      PopulateDocumentAuthorDeleted -> "populate_document_author_deleted"
      UserGroupGarbageCollection    -> "user_group_garbage_collection"

instance PQFormat JobType where
  pqFormat = pqFormat @Text

instance FromSQL JobType where
  type PQBase JobType = PQBase Text
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing ->
        throwM InvalidValue { ivValue = v, ivValidValues = Just $ map snd jobTypeMapper }

instance ToSQL JobType where
  type PQDest JobType = PQBase Text
  toSQL tt = toSQL . fromJust $ tt `lookup` jobTypeMapper

----------------------------------------

data CronJob = CronJob
  { cjType      :: !JobType
  , cjAttempts  :: !Int32
  } deriving (Eq, Ord, Show)

cronJobSelectors :: [SQL]
cronJobSelectors = ["id", "attempts"]

cronJobFetcher :: (JobType, Int32) -> CronJob
cronJobFetcher (jtype, attempts) = CronJob { cjType = jtype, cjAttempts = attempts }

---------------------------------------

cronConsumer
  :: ( CryptoRNG m
     , MonadBase IO m
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadThrow m
     , MonadTime m
     , MonadFileStorage cronenv
     , CryptoRNG cronenv
     , MonadBaseControl IO cronenv
     , MonadDB cronenv
     , MonadIO cronenv
     , MonadLog cronenv
     , MonadMask cronenv
     , MonadReader CronEnv.CronEnv cronenv
     , CryptoRNG dbt
     , MonadBaseControl IO dbt
     , MonadCatch dbt
     , MonadDB dbt
     , MonadIO dbt
     , MonadLog dbt
     , MonadMask dbt
     , MonadThrow dbt
     , MonadTime dbt
     )
  => CronConf
  -> Manager
  -> Maybe (EventProcessor dbt)
  -> Maybe (EventProcessor dbt)
  -> (forall r . cronenv r -> m r)
  -> (forall r . dbt r -> m r)
  -> Int
  -> ConsumerConfig m JobType CronJob
cronConsumer cronConf mgr mmixpanel mplanhat runCronEnv runDB maxRunningJobs =
  ConsumerConfig
    { ccJobsTable           = "cron_jobs"
    , ccConsumersTable      = "cron_workers"
    , ccJobSelectors        = cronJobSelectors
    , ccJobFetcher          = cronJobFetcher
    , ccJobIndex            = cjType
    , ccNotificationChannel = Nothing
    , ccNotificationTimeout = 3 * 1000000
    , ccMaxRunningJobs      = maxRunningJobs
    , ccProcessJob          =
      \CronJob {..} -> logHandlerInfo cjType $ do
        logInfo_ "Processing job"
        startTime <- currentTime
        action    <- case cjType of
          AsyncEventsProcessing -> do
            runDB $ do
              let processMaximum = 20
              asyncProcessEvents getEventProcessor (NoMoreThan processMaximum)
            return . RerunAfter $ iseconds 1
          ClockErrorCollection -> do
            runDB $ collectClockError (cronNtpServers cronConf)
            return . RerunAfter $ ihours 1
          CronStats -> do
            ((), time) <- timed . runDB $ reportCronStats (cronStatsDConf cronConf)
            logInfo "Cron stats generated" $ object ["time" .= time]
            return . RerunAfter $ iminutes 1
          DocumentAutomaticRemindersEvaluation -> do
            runCronEnv expireDocumentAutomaticReminders
            return . RerunAfter $ iminutes 1
          DocumentsAuthorIDMigration -> do
            let batchSize = 1000
            runDB $ do
              ress <- dbUpdate $ UpdateAuthorUserID batchSize
              logInfo "Document author user ID updated" $ object ["items_updated" .= ress]
            now <- currentTime
            if now < todayAtHour 4 now
              then RerunAfter <$> return (iseconds 2)
              else RerunAt . nextDayMidnight <$> currentTime
          DocumentsPurge -> do
            runDB $ do
              (purgedCount, time) <- timed . dbUpdate $ PurgeDocuments 30
              logInfo "Purged documents"
                $ object ["purged" .= purgedCount, "time" .= time]
            RerunAt . nextDayAtHour 2 <$> currentTime
          DocumentsArchiveIdle -> do
            now      <- currentTime
            archived <- runDB $ archiveIdleDocuments now
            logInfo "DocumentsArchiveIdle finished"
              $ object ["total_documents_archived" .= archived]
            RerunAt . nextDayAtHour 19 <$> currentTime
          DocumentSearchUpdate -> do
            numberOfItemsUpdated <- runDB updateHistoricalSearchData
            now                  <- currentTime
            case (numberOfItemsUpdated > 0, now < todayAtHour 4 now) of
              (True, True) -> RerunAfter <$> return (iseconds 1)
              _            -> RerunAt . nextDayMidnight <$> currentTime
          EmailChangeRequestsEvaluation -> do
            runDB . dbUpdate $ DeleteExpiredEmailChangeRequests
            return . RerunAfter $ ihours 1
          FindAndTimeoutDocuments -> do
            runCronEnv findAndTimeoutDocuments
            return . RerunAfter $ iminutes 10
          MailEventsProcessing -> do
            let eventLimit = 50
            eventsDone <- runCronEnv $ Mails.Events.processEvents eventLimit
            let timeDelay = if eventsDone == eventLimit then 1 else 5
            return . RerunAfter $ iseconds timeDelay
          MarkOrphanFilesForPurge -> do
            let -- Share the string between all the log messages.
                orphanFileMarked = "Orphan file marked for purge"
            (fids, time) <-
              timed . runDB . dbUpdate . MarkOrphanFilesForPurgeAfter $ idays 7
            logInfo "Purged files" $ object ["purged" .= length fids, "time" .= time]
            forM_ fids $ \fid -> logInfo orphanFileMarked $ object [identifier fid]
            -- If maximum amount of files was marked, run it again shortly after.
            RerunAt . nextDayAt 2 30 <$> currentTime
          MonthlyInvoice -> do
            case cronMonthlyInvoiceConf cronConf of
              Nothing -> do
                logInfo_ "Monthly-invoice job configuration is missing; skipping"
              Just invoiceConf -> runCronEnv
                $ sendMailWithMonthlyInvoice (cronDBConfig cronConf) invoiceConf
            RerunAt . beginningOfNextMonthAtHour 5 <$> currentTime
          OldDraftsRemoval -> do
            runDB $ do
              delCount <- dbUpdate $ RemoveOldDrafts 100
              logInfo "Removed old, unsaved draft documents"
                $ object ["removed" .= delCount]
            return . RerunAfter $ ihours 1
          OldLogsRemoval -> do
            let connSource ci =
                  simpleSource defaultConnectionSettings { csConnInfo = ci }
                logDBs = catMaybes . for (lcLoggers $ cronLogConfig cronConf) $ \case
                  PostgreSQL ci -> Just ci
                  _             -> Nothing
            forM_ logDBs $ \ci ->
              runDBT (unConnectionSource $ connSource ci) defaultTransactionSettings $ do
                runSQL_ "SELECT current_database()::text"
                dbName :: Text <- fetchOne runIdentity
                n              <- dbUpdate $ CleanLogsOlderThanDays 30
                logInfo "Old logs removed"
                  $ object ["database" .= dbName, "logs_removed" .= n]
            RerunAt . nextDayMidnight <$> currentTime
          PasswordRemindersEvaluation -> do
            runDB . dbUpdate $ DeleteExpiredPasswordReminders
            return . RerunAfter $ ihours 1
          PushPlanhatStats -> do
            case cronPlanhatConf cronConf of
              Nothing -> do
                logInfo "Planhat config missing; skipping" $ object []
              Just phConf -> do
                runDB $ doDailyPlanhatStats phConf mgr
            RerunAt . nextDayAtHour 1 <$> currentTime
          SessionsEvaluation -> do
            runDB . dbUpdate $ DeleteExpiredSessions
            return . RerunAfter $ ihours 1
          SMSEventsProcessing -> do
            runCronEnv $ SMS.Events.processEvents
            return . RerunAfter $ iseconds 5
          UserAccountRequestEvaluation -> do
            runDB expireUserAccountRequests
            return . RerunAfter $ ihours 1
          AttachmentsPurge -> do
            runDB $ do
              purgedCount <- dbUpdate PurgeAttachments
              logInfo "Purged attachments" $ object ["purged" .= purgedCount]
            return . RerunAfter $ iminutes 10
          TimeoutedSignatoryAccessTokensPurge -> do
            runDB $ do
              purgedCount <- dbUpdate PurgeTimeoutedSignatoryAccessTokens
              logInfo "Purged timeouted signatory access tokens"
                $ object ["purged" .= purgedCount]
            return . RerunAfter $ ihours 1
          TemporaryLoginTokensPurge -> do
            runDB $ do
              purgedCount <- dbUpdate PurgeExpiredTemporaryLoginTokens
              logInfo "Purged temporary login tokens" $ object ["purged" .= purgedCount]
            return . RerunAfter $ iminutes 60
          TimeoutedEIDTransactionsPurge -> do
            runDB $ do
              purgedCount <- dbUpdate PurgeTimeoutedEIDTransactions
              logInfo "Purged timeouted eid transactions"
                $ object ["purged" .= purgedCount]
            return . RerunAfter $ iminutes 60
          PopulateDocumentAuthorDeleted -> do
            let
              cs = simpleSource defaultConnectionSettings
                { csConnInfo = cronDBConfig cronConf
                }
              ts = defaultTransactionSettings
                { tsAutoTransaction  = False
                , tsIsolationLevel   = Serializable
                , tsRestartPredicate = Just
                                       $ RestartPredicate
                                       $ \DetailedQueryError { qeErrorCode } _n ->
                                           qeErrorCode == SerializationFailure
                }
              getIDs = "SELECT id FROM documents WHERE author_deleted_filled IS NULL"
            runDBT (unConnectionSource cs) ts $ do
              withCursorSQL "pdad_documents" NoScroll Hold getIDs $ \cursor -> do
                fix $ \loop -> do
                  cursorFetch_ cursor (CD_Forward 1000)
                  dids :: [DocumentID] <- fetchMany runIdentity
                  if null dids
                    then return MarkProcessed
                    else do
                      withTransaction . runQuery_ . sqlUpdate "documents d" $ do
                        sqlSetCmd
                          "(author_deleted, author_really_deleted, author_deleted_filled)"
                          "(SELECT sl.deleted, sl.really_deleted, TRUE FROM signatory_links sl WHERE d.id = sl.document_id AND d.author_id = sl.id)"
                        sqlWhereIn "d.id" dids
                      loop
          UserGroupGarbageCollection -> do
            runDB $ do
              let batchLimit = 1000
              ugsForDeletion <- dbQuery $ FindOldUserGroups batchLimit
              let numMatches = length ugsForDeletion
              forM_ ugsForDeletion $ \ug -> dbUpdate . UserGroupDelete $ ug ^. #id
              logInfo "Marked old User Groups as deleted"
                $ object ["deleted" .= numMatches]
              if numMatches < batchLimit
                then RerunAt . nextDayAtHour 5 <$> currentTime
                else return . RerunAfter $ iminutes 5

        endTime <- currentTime
        logInfo "Job processed successfully" $ object
          ["elapsed_time" .= (realToFrac (diffUTCTime endTime startTime) :: Double)]

        return $ Ok action
    , ccOnException         = \_ CronJob {..} -> return $ case cjAttempts of
                                1 -> RerunAfter $ iminutes 1
                                2 -> RerunAfter $ iminutes 5
                                3 -> RerunAfter $ iminutes 10
                                4 -> RerunAfter $ iminutes 15
                                5 -> RerunAfter $ iminutes 30
                                _ -> RerunAfter $ ihours 1
    }
  where
    logHandlerInfo jobType action =
      localRandomID "job_id" $ localData ["job_type" .= show jobType] action

    getEventProcessor EventMixpanel = mmixpanel
    getEventProcessor EventPlanhat  = mplanhat
