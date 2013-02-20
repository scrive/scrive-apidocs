module ActionQueue.Scheduler (
    Scheduler
  , SchedulerData(..)
  , timeoutDocuments
  , getGlobalTemplates
  ) where

import Control.Concurrent
import Control.Monad.Reader
import System.Time

import ActionQueue.Monad
import AppConf
import DB hiding (update, query)
import Doc.DocStateData
import Doc.Model
import MinutesTime
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
    runReaderT (dbUpdate $ TimeoutDocument (documentid doc) (systemActor now)) gt
    _ <- addDocumentTimeoutStatEvents (documentid doc) "scheduler"
    Log.debug $ "Document timedout " ++ (show $ documenttitle doc)
  when (not (null docs)) $ do
    kCommit
    timeoutDocuments


getGlobalTemplates :: Scheduler KontrakcjaGlobalTemplates
getGlobalTemplates = do
  sd <- ask
  (_, templates) <- liftIO $ readMVar (sdTemplates sd)
  return templates
