module Doc.AutomaticReminder.Model (
    expireDocumentAutomaticReminders
  , setAutomaticReminder
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Data.Int
import Data.Typeable
import Log
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

import CronEnv
import DB
import DB.TimeZoneName
import Doc.DocMails
import Doc.DocumentID
import Doc.DocumentMonad (withDocument)
import Doc.Model
import EventStream.Class
import Log.Identifier
import MinutesTime
import Util.Actor
import qualified DB.TimeZoneName as TimeZoneName

data DocumentAutomaticReminder = DocumentAutomaticReminder
  { darDocumentID :: DocumentID
  , darExpires :: UTCTime
  } deriving (Show,Typeable)

instance Loggable DocumentAutomaticReminder where
  logValue dar = object [identifier $ darDocumentID dar, "sent_time" .= darExpires dar]
  logDefaultLabel _ = "document_automatic_reminder"

setAutomaticReminder
  :: (MonadDB m, MonadTime m, MonadMask m)
  => DocumentID
  -> Maybe Int32
  -> TimeZoneName
  -> m ()
setAutomaticReminder did mdays tzn = do
  void . dbUpdate $ DeleteAutomaticReminder did
  case mdays of
    Nothing   -> return ()
    Just days -> void . dbUpdate $ CreateAutomaticReminder did days tzn

selectAutomaticReminderSelectorsList :: [SQL]
selectAutomaticReminderSelectorsList = ["document_id", "expires"]

expireDocumentAutomaticReminders
  :: ( MonadDB m
     , MonadTime m
     , MonadThrow m
     , MonadReader CronEnv m
     , MonadCatch m
     , MonadLog m
     , MonadIO m
     , CryptoRNG m
     , MonadEventStream m
     )
  => m ()
expireDocumentAutomaticReminders = do
  templates          <- asks ceTemplates
  mailNoreplyAddress <- asks ceMailNoreplyAddress
  dars               <- dbQuery GetExpiredAutomaticReminders
  forM_ dars $ \dar@DocumentAutomaticReminder {..} -> do
    res <- try . localData [identifier darDocumentID] $ do
      now    <- currentTime
      exists <- dbQuery
        $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor darDocumentID
      if exists
        then do
          void $ dbQuery (GetDocumentByDocumentID darDocumentID) >>= \doc ->
            runMailT templates mailNoreplyAddress doc
              . withDocument doc
              $ sendAllReminderEmails (systemActor now) True
        else do
          logInfo
              "Auto reminder dropped since document does not exists or is purged/reallydeleted"
            $ logObject_ dar
      void . dbUpdate $ DeleteAutomaticReminder darDocumentID
    case res of
      Right ()                     -> commit
      Left  (e :: E.SomeException) -> do
        logAttention "DocumentAutomaticReminder sending failed"
          $ object ["exception" .= show e]
        rollback

data GetExpiredAutomaticReminders = GetExpiredAutomaticReminders
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetExpiredAutomaticReminders [DocumentAutomaticReminder] where
  query GetExpiredAutomaticReminders = do
    now <- currentTime
    runQuery_ . sqlSelect "document_automatic_reminders" $ do
      mapM_ sqlResult selectAutomaticReminderSelectorsList
      sqlWhere $ "expires <" <?> now
    fetchMany fetchAutomaticReminder

data CreateAutomaticReminder = CreateAutomaticReminder DocumentID Int32 TimeZoneName
instance (MonadDB m, MonadThrow m, MonadTime m, MonadMask m) => DBUpdate m CreateAutomaticReminder DocumentAutomaticReminder where
  update (CreateAutomaticReminder did days tzn) = withTimeZone defaultTimeZoneName $ do
    time <- currentTime
    let timestamp = formatTime' "%F" time <> " " <> T.unpack (TimeZoneName.toString tzn)
    runQuery_ . sqlInsert "document_automatic_reminders" $ do
      -- send the reminder at 10:15 in the time zone of the document
      sqlSetCmd "expires"
        $   "cast ("
        <?> timestamp
        <+> "as timestamp with time zone)"
        <+> "+ ((interval '1 day') *"
        <?> days
        <+> ") + (interval '10 hours 15 minutes')"
      sqlSet "document_id" did
      mapM_ sqlResult selectAutomaticReminderSelectorsList
    fetchOne fetchAutomaticReminder

newtype DeleteAutomaticReminder = DeleteAutomaticReminder DocumentID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteAutomaticReminder Bool where
  update (DeleteAutomaticReminder did) = do
    runQuery01 . sqlDelete "document_automatic_reminders" $ do
      sqlWhereEq "document_id" did

fetchAutomaticReminder :: (DocumentID, UTCTime) -> DocumentAutomaticReminder
fetchAutomaticReminder (did, expires) =
  DocumentAutomaticReminder { darDocumentID = did, darExpires = expires }
