module ActionQueue.Scheduler (
    Scheduler
  , SchedulerData(..)
  , timeoutDocuments
  , runDocumentProblemsCheck
  , getGlobalTemplates
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.List
import System.Time

import ActionQueue.Monad
import AppConf
import DB hiding (update, query)
import Doc.DocStateData
import Doc.Invariants
import Doc.Model
import MinutesTime
import Mails.MailsData
import Mails.SendMail
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

-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
timeoutDocuments :: Scheduler ()
timeoutDocuments = do
  now <- getMinutesTime
  docs <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 100
  forM_ docs $ \doc -> do
    gt <- getGlobalTemplates
    success <- runReaderT (dbUpdate $ TimeoutDocument (documentid doc) (systemActor now)) gt
    when success $ do
      Just d <- dbQuery $ GetDocumentByDocumentID $ documentid doc
      _ <- addDocumentTimeoutStatEvents d "scheduler"
      return ()
    Log.debug $ "Document timedout " ++ (show $ documenttitle doc)
  when (not (null docs)) $ do
    dbCommit
    timeoutDocuments

runDocumentProblemsCheck :: Scheduler ()
runDocumentProblemsCheck = do
  sd <- ask
  now <- getMinutesTime
  docs <- dbQuery GetAllDocuments
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
