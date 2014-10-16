module Cron.Model (
    WorkerID
  , TaskType(..)
  , ReserveTask(..)
  , UpdateTaskFinishedTime(..)
  , ReleaseTask(..)
  , RegisterWorker(..)
  , UnregisterWorker(..)
  , UpdateWorkerActivity(..)
  , UnregisterWorkersInactiveFor(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Int
import Data.Maybe hiding (fromJust)
import Data.Monoid.Space
import Data.Typeable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import DB
import OurPrelude
import Utils.List

newtype WorkerID = WorkerID Int64
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''WorkerID)

instance FromSQL WorkerID where
  type PQBase WorkerID = PQBase Int64
  fromSQL mbase = WorkerID <$> fromSQL mbase

instance ToSQL WorkerID where
  type PQDest WorkerID = PQDest Int64
  toSQL (WorkerID n) = toSQL n

----------------------------------------

data TaskType
  = AmazonDeletion
  | AmazonUpload
  | AsyncEventsProcessing
  | ClockErrorCollection
  | DocumentAPICallbackEvaluation
  | DocumentAutomaticRemindersEvaluation
  | DocumentsPurge
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
  deriving (Eq, Ord, Show, Typeable)

taskTypeBSRelation :: [(TaskType, BS.ByteString)]
taskTypeBSRelation = [
    (AmazonDeletion, "amazon_deletion")
  , (AmazonUpload, "amazon_upload")
  , (AsyncEventsProcessing, "async_events_processing")
  , (ClockErrorCollection, "clock_error_collection")
  , (DocumentAPICallbackEvaluation, "document_api_callback_evaluation")
  , (DocumentAutomaticRemindersEvaluation, "document_automatic_reminders_evaluation")
  , (DocumentsPurge, "documents_purge")
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

instance PQFormat TaskType where
  pqFormat _ = pqFormat (undefined::BS.ByteString)

instance FromSQL TaskType where
  type PQBase TaskType = PQBase BS.ByteString
  fromSQL mbase = do
    v <- fromSQL mbase
    case v `rlookup` taskTypeBSRelation of
      Just tt -> return tt
      Nothing -> E.throwIO $ InvalidValue {
        ivValue = v
      , ivValidValues = Just $ map snd taskTypeBSRelation
      }

instance ToSQL TaskType where
  type PQDest TaskType = PQBase BS.ByteString
  toSQL tt = toSQL . $fromJust $ tt `lookup` taskTypeBSRelation

----------------------------------------

data ReserveTask = ReserveTask WorkerID
instance (MonadDB m, MonadThrow m) => DBUpdate m ReserveTask (Maybe TaskType) where
  update (ReserveTask wid) = do
    runQuery_ . sqlSelect "cron_tasks" $ do
      sqlResult "type"
      sqlWhereIsNULL "worker_id"
      sqlWhere "finished IS NULL OR finished + frequency <= now()"
      sqlLimit 1
    tt <- fetchMaybe unSingle
    when (isJust tt) . runQuery_ . sqlUpdate "cron_tasks" $ do
      sqlSetCmd "started" "now()"
      sqlSet "worker_id" wid
      sqlWhereEq "type" tt
    return tt

data UpdateTaskFinishedTime = UpdateTaskFinishedTime TaskType
instance MonadDB m => DBUpdate m UpdateTaskFinishedTime () where
  update (UpdateTaskFinishedTime tt) =
    runQuery_ . sqlUpdate "cron_tasks" $ do
      sqlSetCmd "finished" "now()"
      sqlSet "worker_id" (Nothing :: Maybe WorkerID)
      sqlWhereEq "type" tt

data ReleaseTask = ReleaseTask TaskType
instance MonadDB m => DBUpdate m ReleaseTask () where
  update (ReleaseTask tt) =
    runQuery_ . sqlUpdate "cron_tasks" $ do
      sqlSet "worker_id" (Nothing :: Maybe WorkerID)
      sqlWhereEq "type" tt

----------------------------------------

data RegisterWorker = RegisterWorker
instance (MonadDB m, MonadThrow m) => DBUpdate m RegisterWorker WorkerID where
  update RegisterWorker = do
    runQuery_ . sqlInsert "cron_workers" $ do
      sqlSetCmd "last_activity" "now()"
      sqlResult "id"
    fetchOne unSingle

data UnregisterWorker = UnregisterWorker WorkerID
instance MonadDB m => DBUpdate m UnregisterWorker () where
  update (UnregisterWorker wid) =
    runQuery_ . sqlDelete "cron_workers" $ sqlWhereEq "id" wid

data UpdateWorkerActivity = UpdateWorkerActivity WorkerID
instance MonadDB m => DBUpdate m UpdateWorkerActivity () where
  update (UpdateWorkerActivity wid) =
    runQuery_ . sqlUpdate "cron_workers" $ do
      sqlSetCmd "last_activity" "now()"
      sqlWhereEq "id" wid

data UnregisterWorkersInactiveFor = UnregisterWorkersInactiveFor Interval
instance MonadDB m => DBUpdate m UnregisterWorkersInactiveFor Int where
  update (UnregisterWorkersInactiveFor int) =
    runQuery . sqlDelete "cron_workers" $
      sqlWhere $ "last_activity +" <?> int <+> "<= now()"
