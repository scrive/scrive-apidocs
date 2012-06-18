module ActionQueue.Scheduler (
    Scheduler
  , SchedulerData(..)
  , oldScheduler
  , runDocumentProblemsCheck
  , getGlobalTemplates
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.List
import System.Time

import ActionQueue.Monad
import AppControl (AppConf(..))
import DB hiding (update, query)
import Doc.DocStateData
import Doc.Invariants
import Doc.Model
import MinutesTime
import Mails.MailsData
import Mails.SendMail
import Session
import Stats.Control
import Templates.TemplatesLoader
import Util.Actor
import qualified Log

data SchedulerData = SchedulerData {
    sdAppConf   :: AppConf
  , sdTemplates :: MVar (ClockTime, KontrakcjaGlobalTemplates)
  }

type Scheduler = ActionQueue SchedulerData

-- Note: Do not define TemplatesMonad instance for Scheduler, use
-- TemplatesT instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

-- | Old scheduler (used as main one before action scheduler was implemented)
oldScheduler :: Scheduler ()
oldScheduler = do
  now <- getMinutesTime
  timeoutDocuments now
  dropExpiredSessions now
  where
    timeoutDocuments now = do
      docs <- dbQuery $ GetTimeoutedButPendingDocuments now
      forM_ docs $ \doc -> do
        gt <- getGlobalTemplates
        edoc <- runReaderT (dbUpdate $ TimeoutDocument (documentid doc) (systemActor now)) gt
        case edoc of
          Left _ -> return ()
          Right doc' -> do
            _ <- addDocumentTimeoutStatEvents doc' "scheduler"
            return ()
        Log.debug $ "Document timedout " ++ (show $ documenttitle doc)

runDocumentProblemsCheck :: Scheduler ()
runDocumentProblemsCheck = do
  sd <- ask
  now <- getMinutesTime
  docs <- dbQuery $ GetDocumentsByService Nothing
  let probs = listInvariantProblems now docs
  when (probs /= []) $ mailDocumentProblemsCheck $
    "<p>"  ++ (hostpart $ sdAppConf sd) ++ "/dave/document/" ++
    intercalate ("</p>\n\n<p>" ++ (hostpart $ sdAppConf sd) ++ "/dave/document/") probs ++
    "</p>"
  return ()
  where
    -- | Send an email out to all registered emails about document problems.
    mailDocumentProblemsCheck msg = do
      sd <- ask
      scheduleEmailSendout (mailsConfig $ sdAppConf sd) $ Mail {
          to = zipWith MailAddress documentProblemsCheckEmails documentProblemsCheckEmails
        , title = "Document problems report " ++ (hostpart $ sdAppConf sd)
        , content = msg
        , attachments = []
        , from = Nothing
        , mailInfo = None
        }

    -- | A message will be sent to these email addresses when
    -- there is an inconsistent document found in the database.
    documentProblemsCheckEmails = ["bugs@skrivapa.se"]

getGlobalTemplates :: Scheduler KontrakcjaGlobalTemplates
getGlobalTemplates = do
  sd <- ask
  (_, templates) <- liftIO $ readMVar (sdTemplates sd)
  return templates
