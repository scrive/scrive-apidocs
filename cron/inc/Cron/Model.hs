module Cron.Model (
    JobType(..)
  , CronJob(..)
  , cronJobSelectors
  , cronJobFetcher
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Int
import Database.PostgreSQL.PQTypes

import OurPrelude
import Utils.List

data JobType
  = AmazonDeletion
  | AmazonUpload
  | AsyncEventsProcessing
  | ClockErrorCollection
  | DocumentAutomaticRemindersEvaluation
  | DocumentsPurge
  | DocumentsArchiveIdle
  | EmailChangeRequestsEvaluation
  | FindAndDoPostDocumentClosedActions
  | FindAndDoPostDocumentClosedActionsNew
  | FindAndExtendDigitalSignatures
  | FindAndTimeoutDocuments
  | MailEventsProcessing
  | OldDraftsRemoval
  | PasswordRemindersEvaluation
  | RecurlySynchronization
  | SessionsEvaluation
  | SMSEventsProcessing
  | UserAccountRequestEvaluation
  deriving (Eq, Ord, Show)

jobTypeMapper :: [(JobType, ByteString)]
jobTypeMapper = [
    (AmazonDeletion, "amazon_deletion")
  , (AmazonUpload, "amazon_upload")
  , (AsyncEventsProcessing, "async_events_processing")
  , (ClockErrorCollection, "clock_error_collection")
  , (DocumentAutomaticRemindersEvaluation, "document_automatic_reminders_evaluation")
  , (DocumentsPurge, "documents_purge")
  , (DocumentsArchiveIdle, "documents_archive_idle")
  , (EmailChangeRequestsEvaluation, "email_change_requests_evaluation")
  , (FindAndDoPostDocumentClosedActions, "find_and_do_post_document_closed_actions")
  , (FindAndDoPostDocumentClosedActionsNew, "find_and_do_post_document_closed_actions_new")
  , (FindAndExtendDigitalSignatures, "find_and_extend_digital_signatures")
  , (FindAndTimeoutDocuments, "find_and_timeout_documents")
  , (MailEventsProcessing, "mail_events_processing")
  , (OldDraftsRemoval, "old_drafts_removal")
  , (PasswordRemindersEvaluation, "password_reminders_evaluation")
  , (RecurlySynchronization, "recurly_synchronization")
  , (SessionsEvaluation, "sessions_evaluation")
  , (SMSEventsProcessing, "sms_events_processing")
  , (UserAccountRequestEvaluation, "user_account_request_evaluation")
  ]

instance PQFormat JobType where
  pqFormat _ = pqFormat (undefined::ByteString)

instance FromSQL JobType where
  type PQBase JobType = PQBase ByteString
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` jobTypeMapper of
      Just tt -> return tt
      Nothing -> throwM InvalidValue {
        ivValue = v
      , ivValidValues = Just $ map snd jobTypeMapper
      }

instance ToSQL JobType where
  type PQDest JobType = PQBase ByteString
  toSQL tt = toSQL . $fromJust $ tt `lookup` jobTypeMapper

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
