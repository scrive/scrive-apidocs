module Doc.AutomaticReminder.Model (
    documentAutomaticReminder
  ) where

--import Control.Applicative
import Control.Monad
import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import DB.SQL2
import Doc.DocumentID
import MinutesTime
import Doc.Model
import Doc.AutomaticReminder.Tables
import AppConf
import Control.Monad.Trans
import Context
import Doc.DocStateData
import Data.Functor
import User.Model
import Control.Monad.Reader
import Util.SignatoryLinkUtils
import BrandedDomains
import Data.Maybe
import Doc.DocMails
import Util.Actor
import IPAddress (noIP)
import qualified Log as Log
import Crypto.RNG

data DocumentAutomaticReminder = DocumentAutomaticReminder {
    reminderDocumentID :: DocumentID
  , reminderSentTime :: MinutesTime
  } deriving (Show)


documentAutomaticReminder :: Action DocumentID DocumentAutomaticReminder (DocumentID,MinutesTime) Scheduler
documentAutomaticReminder = Action {
    qaTable = tableDocumentAutomaticReminders
  , qaFields = \(did,stime) -> [
        ("document_id", toSql did)
      , ("expires", toSql stime)
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
    sentReminder :: (Log.MonadLog m, CryptoRNG m, MonadDB m, MonadIO m, MonadReader SchedulerData m) => DocumentAutomaticReminder -> m ()
    sentReminder dar = do
      now <- getMinutesTime
      appConf <- asks sdAppConf
      doc <- dbQuery $ GetDocumentByDocumentID (reminderDocumentID dar)
      mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) $ join $ maybesignatory <$> getAuthorSigLink doc
      let mbd = flip findBrandedDomain (brandedDomains appConf) =<< userassociateddomain =<< mauthor
      let mctx = MailContext {   mctxhostpart = fromMaybe (hostpart appConf) (bdurl <$> mbd)
                               , mctxmailsconfig = mailsConfig appConf
                               , mctxlang = documentlang doc
                               , mctxcurrentBrandedDomain = mbd
                               , mctxipnumber = noIP
                               , mctxtime = now
                               , mctxmaybeuser = Nothing
                               }
      gt <- getGlobalTemplates
      _ <- runReaderT (sendAllReminderEmails mctx (systemActor now) (reminderDocumentID dar))  gt
      void $ dbUpdate $ DeleteAction documentAutomaticReminder (reminderDocumentID dar)



