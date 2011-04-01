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

type SchedulerData' = SchedulerData AppConf Mailer KontrakcjaTemplates

newtype ActionScheduler a = AS { unActionScheduler :: ReaderT SchedulerData' IO a }
    deriving (Monad, Functor, MonadIO, MonadReader SchedulerData')

runScheduler :: ActionScheduler a -> SchedulerData' -> IO a
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
         PasswordReminder _ _ _ -> deleteAction $ actionID action
         ViralInvitationSent _ _ _ _ _ -> deleteAction $ actionID action
         AccountCreated _ _ -> deleteAction $ actionID action
         AccountCreatedBySigning state uid (docid, _) token -> do
             let aid = actionID action
             now <- liftIO getMinutesTime
             case state of
                  JustCreated ->
                      sendReminder mailAccountCreatedBySigningReminder FirstReminderSent ((3*24*60) `minutesAfter` now) aid uid docid token
                  FirstReminderSent ->
                      sendReminder mailAccountCreatedBySigningReminder SecondReminderSent ((3*24*60) `minutesAfter` now) aid uid docid token
                  SecondReminderSent ->
                      sendReminder mailAccountCreatedBySigningLastReminder ThirdReminderSent ((24*60) `minutesAfter` now) aid uid docid token
                  ThirdReminderSent -> deleteAction $ actionID action
    where
        deleteAction aid = do
            _ <- update $ DeleteAction aid
            return ()

        sendReminder reminder new_state new_evaltime aid uid docid token = do
            sd <- ask
            doctitle <- (query $ GetDocumentByDocumentID docid) >>= maybe (return BS.empty) (return . documenttitle)
            (query $ GetUserByUserID uid) >>= maybe (return ()) (\user -> do
                let uinfo = userinfo user
                    email = useremail uinfo
                    fullname = userfullname user
                mail <- liftIO $ reminder (sdTemplates sd) (hostpart $ sdAppConf sd) doctitle fullname (LinkAccountCreatedBySigning aid token) (LinkAccountRemoval aid token)
                liftIO $ sendMail (sdMailer sd) $ mail { fullnameemails = [(fullname, unEmail email)] })
            let new_atype = (actionType action) { acbsState = new_state }
            _ <- update $ UpdateActionType aid new_atype
            _ <- update $ UpdateActionEvalTime aid new_evaltime
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

