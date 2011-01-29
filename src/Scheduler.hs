-- |Simple session support
module Scheduler
    ( runScheduler )
    where

import System.IO
import DocState
import Happstack.State (query, update)
import Control.Monad
import MinutesTime
import System.Log.Logger
import AppControl
import Session

-- | Run the scheduler
runScheduler appconfig = do
  now <- getMinutesTime
  timeoutDocuments now
  dropExpiredSessions now
  debugM "Happstack.Server" $ "Scheduler is running ..."

-- | Timeout all old documents
timeoutDocuments now = do
  docs <- query $ GetTimeoutedButPendingDocuments now
  forM_ docs $ \doc -> do 
                     update $ TimeoutDocument (documentid doc) now 
                     debugM "Happstack.Server" $ "Document timedout " ++ (show $ documenttitle doc)
