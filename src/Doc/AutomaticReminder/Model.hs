module Doc.AutomaticReminder.Model (
    documentAutomaticReminder
  , scheduleAutoreminderIfThereIsOne
  , setAutoreminder
  ) where

--import Control.Applicative
import Control.Monad
import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import DB.SQL2
import Doc.DocumentID
import Doc.DocumentMonad (withDocument)
import MinutesTime
import Doc.Model
import Doc.AutomaticReminder.Tables
import Doc.DocStateData
import Control.Monad.Reader
import Doc.DocMails
import Util.Actor
import qualified Log
import Crypto.RNG
import Data.Typeable
import DB.TimeZoneName (TimeZoneName, mkTimeZoneName, withTimeZone)
import qualified DB.TimeZoneName as TimeZoneName
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base

data DocumentAutomaticReminder = DocumentAutomaticReminder {
    reminderDocumentID :: DocumentID
  , reminderSentTime :: MinutesTime
  } deriving (Show,Typeable)


documentAutomaticReminder :: Action DocumentID DocumentAutomaticReminder DocumentID Scheduler
documentAutomaticReminder = Action {
    qaTable = tableDocumentAutomaticReminders
  , qaFields = \(did) -> [
        ("document_id", toSql did)
     ]
  , qaSelectFields = ["document_id", "expires"]
  , qaIndexField = "document_id"
  , qaExpirationDelay = "5 minutes" -- not really needed
  , qaDecode = kFold decoder []
  , qaUpdateSQL = \DocumentAutomaticReminder{..} -> toSQLCommand $ sqlUpdate "document_automatic_reminders" $ do
      sqlSet "expires" reminderSentTime
      sqlWhereEq (qaIndexField documentAutomaticReminder) reminderDocumentID
  , qaEvaluateExpired = sentReminder
  }
  where
    decoder acc document_id stime = DocumentAutomaticReminder {
        reminderDocumentID = document_id
      , reminderSentTime = stime
      } : acc
    sentReminder :: (Log.MonadLog m, CryptoRNG m, MonadDB m, MonadBase IO m, MonadReader SchedulerData m) => DocumentAutomaticReminder -> m ()
    sentReminder dar = do
      now <- getMinutesTime
      _ <- dbQuery (GetDocumentByDocumentID (reminderDocumentID dar)) >>= \doc -> runMailTInScheduler doc $
        withDocument doc $ sendAllReminderEmails (systemActor now) True
      void $ dbUpdate $ DeleteAction documentAutomaticReminder (reminderDocumentID dar)



scheduleAutoreminderIfThereIsOne :: (MonadDB m, MonadBaseControl IO m) => TimeZoneName -> Document -> m ()
scheduleAutoreminderIfThereIsOne tzn doc = setAutoreminder (documentid doc) (documentdaystoremind doc)  tzn

setAutoreminder :: (MonadDB m, MonadBaseControl IO m) => DocumentID -> Maybe Int -> TimeZoneName -> m ()
setAutoreminder did mdays tzn = do
      void $  dbUpdate $ DeleteAction documentAutomaticReminder did
      case (mdays) of
        Nothing   -> return ()
        Just days -> do
            time <- getMinutesTime
            let timestamp = formatTime defaultTimeLocale "%F" (toUTCTime time) ++ " " ++ TimeZoneName.toString tzn
            dstTz <- mkTimeZoneName "Europe/Stockholm"
            withTimeZone dstTz $
              void $ kRun $ sqlInsert "document_automatic_reminders" $ do
                sqlSetCmd "expires" $ "cast (" <?> timestamp <+> "as timestamp with time zone)"
                                <+> "+ ((interval '1 day') * " <?> (show days) <+> " ) + (interval '7 hours 30 minutes')"
                sqlSet "document_id" did
