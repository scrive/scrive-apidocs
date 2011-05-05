{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module ActionScheduler (
      ActionScheduler
    , runScheduler
    , runEnforceableScheduler
    , actionScheduler
    , oldScheduler
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Maybe (isJust)
import Happstack.State (query, update)
import System.Log.Logger (debugM)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import AppControl (AppConf(..))
import ActionSchedulerState
import Doc.DocState
import Kontra
import KontraLink
import MinutesTime
import Mails.SendMail
import Session
import Templates.Templates (KontrakcjaTemplates)
import User.UserView
import qualified AppLogger as Log
import Control.Concurrent.MVar
import System.Time

type SchedulerData' = SchedulerData AppConf Mailer (MVar (ClockTime, KontrakcjaTemplates))

newtype ActionScheduler a = AS { unActionScheduler :: ReaderT SchedulerData' IO a }
    deriving (Monad, Functor, MonadIO, MonadReader SchedulerData')

runScheduler :: ActionScheduler () -> SchedulerData' -> IO ()
runScheduler sched sd =
    runReaderT (unActionScheduler sched) sd `E.catch` catchEverything
    where
        catchEverything :: E.SomeException -> IO ()
        catchEverything e =
            Log.error $ "Oops, scheduler error: " ++ show e

-- | Creates scheduler that may be forced to look up for actions to execute
runEnforceableScheduler :: Int -> MVar () -> ActionScheduler () -> SchedulerData' -> IO ()
runEnforceableScheduler interval enforcer sched sd = listen 0
    where
        listen delay = do
            run_now <- tryTakeMVar enforcer
            if isJust run_now || delay >= interval
               then runScheduler sched sd >> listen 0
               else threadDelay 1000000 >> (listen $! delay+1)

-- | Gets 'expired' actions and evaluates them
actionScheduler :: ActionImportance -> ActionScheduler ()
actionScheduler imp = do
        liftIO getMinutesTime
    >>= query . GetExpiredActions imp
    >>= sequence_ . map evaluateAction

-- | Evaluates one action depending on its type
evaluateAction :: Action -> ActionScheduler ()
evaluateAction Action{actionType = TrustWeaverUpload{}} =
    error "TrustWeaverUpload not yet implemented"

evaluateAction Action{actionType = AmazonUpload{}} =
    error "AmazonUpload not yet implemented"

evaluateAction Action{actionID, actionType = PasswordReminder{}} =
    deleteAction actionID

evaluateAction Action{actionID, actionType = ViralInvitationSent{}} =
    deleteAction actionID

evaluateAction Action{actionID, actionType = AccountCreated{}} =
    deleteAction actionID

evaluateAction action@Action{actionID, actionType = AccountCreatedBySigning state uid (docid, _) token} = do
    case state of
         NothingSent ->
             sendReminder 
         ReminderSent ->
             deleteAction actionID
    where
        sendReminder = do
            now <- liftIO getMinutesTime
            sd <- ask
            mdoc <- query $ GetDocumentByDocumentID docid
            let doctitle = maybe BS.empty documenttitle mdoc
            (query $ GetUserByUserID uid) >>= maybe (return ()) (\user -> do
                let uinfo = userinfo user
                    email = useremail uinfo
                    fullname = userfullname user
                (_,templates) <- liftIO $ readMVar (sdTemplates sd)
                let mailfunc = case mdoc of
                      (Just doc) | isOffer doc -> mailAccountCreatedBySigningOfferReminder
                      _ -> mailAccountCreatedBySigningContractReminder
                mail <- liftIO $ mailfunc templates (hostpart $ sdAppConf sd) doctitle fullname (LinkAccountCreatedBySigning actionID token)
                liftIO $ sendMail (sdMailer sd) $ mail { fullnameemails = [(fullname, unEmail email)] }
                return ())
            let new_atype = (actionType action) { acbsState = ReminderSent }
            update $ UpdateActionType actionID new_atype
            update $ UpdateActionEvalTime actionID ((72 * 60) `minutesAfter` now)
            return ()

evaluateAction Action{actionID, actionType = EmailSendout mail} = do
    mailer <- sdMailer <$> ask
    success <- liftIO $ sendMail mailer mail
    if success
       then deleteAction actionID
       else do
           now <- liftIO $ getMinutesTime
           update $ UpdateActionEvalTime actionID $ 5 `minutesAfter` now
           return ()

deleteAction :: ActionID -> ActionScheduler ()
deleteAction aid = do
    update $ DeleteAction aid
    return ()

-- | Old scheduler
oldScheduler :: ActionScheduler ()
oldScheduler = do
    now <- liftIO getMinutesTime
    timeoutDocuments now
    dropExpiredSessions now
    liftIO $ debugM "Happstack.Server" $ "Scheduler is running ..."

timeoutDocuments :: MinutesTime -> ActionScheduler ()
timeoutDocuments now = do
    docs <- query $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do 
        update $ TimeoutDocument (documentid doc) now 
        liftIO $ debugM "Happstack.Server" $ "Document timedout " ++ (show $ documenttitle doc)

