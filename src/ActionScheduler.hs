{-# OPTIONS_GHC -Wall #-}

module ActionScheduler (
      ActionScheduler
    , runScheduler
    , actionScheduler
    , mainScheduler
    ) where

import Control.Monad.Reader
import Happstack.State (query, update)
import System.Log.Logger (debugM)

import AppControl (AppConf)
import ActionSchedulerState
import Doc.DocState
import MinutesTime
import Session

newtype ActionScheduler a = ActionScheduler { unActionScheduler :: ReaderT AppConf IO a }
    deriving (Monad, MonadIO, MonadReader AppConf)

runScheduler :: ActionScheduler a -> AppConf -> IO a
runScheduler = runReaderT . unActionScheduler

-- | Gets 'expired' actions and evaluates them
actionScheduler :: ActionImportance -> ActionScheduler ()
actionScheduler imp = do
        liftIO getMinutesTime
    >>= query . GetExpiredActions imp
    >>= sequence_ . map evaluateAction

evaluateAction :: Action -> ActionScheduler ()
evaluateAction action =
    case actionType action of
         TrustWeaverUpload _ _ -> error "TrustWeaverUpload not yet implemented"
         AmazonUpload _ _ -> error "AmazonUpload not yet implemented"
         PasswordReminder _ _ -> deleteAction $ actionID action
         AccountCreated _ _ -> deleteAction $ actionID action
         AccountCreatedBySigning _ _ _ -> error "AccountCreatedBySigning not yet implemented"
    where
        deleteAction aid = do
        _ <- update $ DeleteAction aid
        return ()

-- | Old scheduler
mainScheduler :: ActionScheduler ()
mainScheduler = do
    now <- liftIO $ getMinutesTime
    timeoutDocuments now
    dropExpiredSessions now
    liftIO $ debugM "Happstack.Server" $ "Scheduler is running ..."

timeoutDocuments :: MinutesTime -> ActionScheduler ()
timeoutDocuments now = do
    docs <- query $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do 
        _ <- update $ TimeoutDocument (documentid doc) now 
        liftIO $ debugM "Happstack.Server" $ "Document timedout " ++ (show $ documenttitle doc)

