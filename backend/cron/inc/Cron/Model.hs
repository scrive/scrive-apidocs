module Cron.Model (
    JobType(..)
  , CronJob(..)
  , cronJobSelectors
  , cronJobFetcher
  ) where

import Control.Monad.Catch
import Data.Int
import Database.PostgreSQL.PQTypes
import qualified Data.Text as T

import KontraPrelude
import Utils.List

data JobType
  = AmazonUpload
  | AsyncEventsProcessing
  | ClockErrorCollection
  | DocumentAutomaticRemindersEvaluation
  | DocumentsPurge
  | DocumentsArchiveIdle
  | EmailChangeRequestsEvaluation
  | FindAndDoPostDocumentClosedActions
  | FindAndExtendDigitalSignatures
  | FindAndTimeoutDocuments
  | MailEventsProcessing
  | MarkOrphanFilesForPurge
  | OldDraftsRemoval
  | OldLogsRemoval
  | PasswordRemindersEvaluation
  | PurgeOrphanFile
  | SessionsEvaluation
  | SMSEventsProcessing
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
  , (FindAndDoPostDocumentClosedActions, "find_and_do_post_document_closed_actions")
  , (FindAndExtendDigitalSignatures, "find_and_extend_digital_signatures")
  , (FindAndTimeoutDocuments, "find_and_timeout_documents")
  , (MailEventsProcessing, "mail_events_processing")
  , (MarkOrphanFilesForPurge, "mark_orphan_files_for_purge")
  , (OldDraftsRemoval, "old_drafts_removal")
  , (OldLogsRemoval, "old_logs_removal")
  , (PasswordRemindersEvaluation, "password_reminders_evaluation")
  , (PurgeOrphanFile, "purge_orphan_file")
  , (SessionsEvaluation, "sessions_evaluation")
  , (SMSEventsProcessing, "sms_events_processing")
  , (UserAccountRequestEvaluation, "user_account_request_evaluation")
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
