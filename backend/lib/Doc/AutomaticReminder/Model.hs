module Doc.AutomaticReminder.Model (
    documentAutomaticReminder
  , scheduleAutoreminderIfThereIsOne
  , setAutoreminder
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Data.Int
import Data.Typeable
import Log

import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import DB.TimeZoneName (TimeZoneName, defaultTimeZoneName, withTimeZone)
import Doc.AutomaticReminder.Tables
import Doc.DocMails
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (withDocument)
import Doc.Model
import KontraPrelude
import Log.Identifier
import MinutesTime
import Util.Actor
import qualified DB.TimeZoneName as TimeZoneName

data DocumentAutomaticReminder = DocumentAutomaticReminder {
    reminderDocumentID :: DocumentID
  , reminderSentTime :: UTCTime
  } deriving (Show,Typeable)

instance LogObject DocumentAutomaticReminder where
  logObject dar = object [ identifier_ $ reminderDocumentID dar ]

instance LogDefaultLabel DocumentAutomaticReminder where
  logDefaultLabel _ = "document_automatic_reminder"

documentAutomaticReminder :: Action DocumentID DocumentAutomaticReminder DocumentID Scheduler
documentAutomaticReminder = Action {
    qaTable = tableDocumentAutomaticReminders
  , qaSetFields = \did -> do
      sqlSet "document_id" did
  , qaSelectFields = ["document_id", "expires"]
  , qaIndexField = "document_id"
  , qaExpirationDelay = "5 minutes" -- not really needed
  , qaDecode = \(document_id, stime) -> DocumentAutomaticReminder {
      reminderDocumentID = document_id
    , reminderSentTime = stime
    }
  , qaUpdateSQL = \DocumentAutomaticReminder{..} -> toSQLCommand $ sqlUpdate "document_automatic_reminders" $ do
      sqlSet "expires" reminderSentTime
      sqlWhereEq (qaIndexField documentAutomaticReminder) reminderDocumentID
  , qaEvaluateExpired = sentReminder
  }
  where
    sentReminder :: (MonadLog m, MonadCatch m, CryptoRNG m, MonadDB m, MonadIO m, MonadReader SchedulerData m) => DocumentAutomaticReminder -> m ()
    sentReminder dar = do
      now <- currentTime
      exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor $ reminderDocumentID dar
      if exists
        then do
          void $ dbQuery (GetDocumentByDocumentID (reminderDocumentID dar)) >>= \doc -> runMailTInScheduler doc $
            withDocument doc $ sendAllReminderEmails (systemActor now) True
        else do
          logInfo "Auto reminder dropped since document does not exists or is purged/reallydeleted" $ logObject dar
      void $ dbUpdate $ DeleteAction documentAutomaticReminder (reminderDocumentID dar)

scheduleAutoreminderIfThereIsOne :: (MonadDB m, MonadTime m, MonadMask m) => TimeZoneName -> Document -> m ()
scheduleAutoreminderIfThereIsOne tzn doc = setAutoreminder (documentid doc) (documentdaystoremind doc) tzn

setAutoreminder :: (MonadDB m, MonadTime m, MonadMask m) => DocumentID -> Maybe Int32 -> TimeZoneName -> m ()
setAutoreminder did mdays tzn = do
      void $  dbUpdate $ DeleteAction documentAutomaticReminder did
      case mdays of
        Nothing   -> return ()
        Just days -> do
            time <- currentTime
            let timestamp = formatTime' "%F" time ++ " " ++ TimeZoneName.toString tzn
            withTimeZone defaultTimeZoneName $
              void . runQuery_ . sqlInsert "document_automatic_reminders" $ do
                sqlSetCmd "expires" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                                <+> "+ ((interval '1 day') * " <?> days <+> " ) + (interval '10 hours 15 minutes')"
                sqlSet "document_id" did
