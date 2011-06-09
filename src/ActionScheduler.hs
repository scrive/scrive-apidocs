{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-do-bind #-}

module ActionScheduler (
      ActionScheduler
    , runScheduler
    , runEnforceableScheduler
    , actionScheduler
    , oldScheduler
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Maybe
import Happstack.State
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import AppControl (AppConf(..))
import ActionSchedulerState
import Doc.DocState
import Kontra
import KontraLink
import MinutesTime
import Mails.MailsData
import Mails.SendMail
import Session
import Templates.Templates (KontrakcjaTemplates)
import User.UserView
import qualified AppLogger as Log
import System.Time

type SchedulerData' = SchedulerData AppConf Mailer (MVar (ClockTime, KontrakcjaTemplates))

type ActionScheduler a = ReaderT SchedulerData' IO a 

runScheduler :: ActionScheduler () -> SchedulerData' -> IO ()
runScheduler sched sd =
    runReaderT sched sd `E.catch` catchEverything
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
                scheduleEmailSendout (sdMailEnforcer sd) $ mail { to = [MailAddress {fullname = fullname, email = unEmail email}] })
            let new_atype = (actionType action) { acbsState = ReminderSent }
            update $ UpdateActionType actionID new_atype
            update $ UpdateActionEvalTime actionID ((72 * 60) `minutesAfter` now)
            return ()

evaluateAction Action{actionID, actionType = EmailSendout mail@Mail{mailInfo}} = do
    mailer <- sdMailer <$> ask
    success <- liftIO $ sendMail mailer actionID mail
    if success
       then do
           -- morph action type into SentEmailInfo
           let email' = email (head (to mail))
           now <- liftIO getMinutesTime
           update $ UpdateActionType actionID $ SentEmailInfo {
                 seiEmail            = Email email'
               , seiMailInfo         = mailInfo
               , seiEventType        = Other "passed to sendgrid"
               , seiLastModification = now
           }
           update $ UpdateActionEvalTime actionID $ (60*24*30) `minutesAfter` now
           return ()
       else do
           now <- liftIO $ getMinutesTime
           update $ UpdateActionEvalTime actionID $ 5 `minutesAfter` now
           return ()

evaluateAction Action{actionID, actionType = SentEmailInfo{}} = do
    deleteAction actionID

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
    deleteQuarantinedDocuments now
    Log.debug $ "Scheduler is running ..."

timeoutDocuments :: MinutesTime -> ActionScheduler ()
timeoutDocuments now = do
    docs <- query $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do 
        update $ TimeoutDocument (documentid doc) now 
        Log.debug $ "Document timedout " ++ (show $ documenttitle doc)

deleteQuarantinedDocuments :: MinutesTime -> ActionScheduler ()
deleteQuarantinedDocuments now = do
    docs <- query $ GetExpiredQuarantinedDocuments now
    forM_ docs $ \doc -> do
        update $ EndQuarantineForDocument (documentid doc)
        Log.debug $ "Document quarantine expired " ++ (show $ documenttitle doc)

