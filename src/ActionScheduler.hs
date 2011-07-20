module ActionScheduler (
      ActionScheduler
    , runScheduler
    , runEnforceableScheduler
    , actionScheduler
    , oldScheduler
    ) where

import Control.Applicative
import Control.Arrow
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
import Templates.LocalTemplates
import Templates.Templates
import User.UserView
import qualified AppLogger as Log
import System.Time
import Util.HasSomeUserInfo

type SchedulerData' = SchedulerData AppConf Mailer (MVar (ClockTime, KontrakcjaMultilangTemplates))

newtype ActionScheduler a = AS (ReaderT SchedulerData' IO a)
    deriving (Monad, Functor, MonadIO, MonadReader SchedulerData')

-- Note: Do not define TemplatesMonad instance for ActionScheduler, use
-- LocalTemplates instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

runScheduler :: ActionScheduler () -> SchedulerData' -> IO ()
runScheduler (AS sched) sd = runReaderT sched sd

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
    sd <- ask
    liftIO $ getMinutesTime
         >>= query . GetExpiredActions imp
         >>= sequence_ . map (run sd)
    where
        run sd a = runScheduler (evaluateAction a) sd `E.catch` catchEverything a
        catchEverything :: Action -> E.SomeException -> IO ()
        catchEverything a e =
            Log.error $ "Oops, evaluateAction with " ++ show a ++ " failed with error: " ++ show e

-- | Old scheduler (used as main one before action scheduler was implemented)
oldScheduler :: ActionScheduler ()
oldScheduler = do
    now <- liftIO getMinutesTime
    timeoutDocuments now
    dropExpiredSessions now

-- Internal stuff

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

evaluateAction Action{actionID, actionType = AccountCreatedBySigning state uid doclinkdataid@(docid, _) token} = do
    case state of
         NothingSent ->
             sendReminder
         ReminderSent ->
             deleteAction actionID
    where
        sendReminder :: ActionScheduler ()
        sendReminder = do
            now <- liftIO getMinutesTime
            sd <- ask
            mdoc <- query $ GetDocumentByDocumentID docid
            let doctitle = maybe BS.empty documenttitle mdoc
            (query $ GetUserByUserID uid) >>= maybe (return ()) (\user -> do
                let mailfunc :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
                    mailfunc = case documenttype <$> mdoc of
                      Just (Signable Offer) -> mailAccountCreatedBySigningOfferReminder
                      Just (Signable Contract) -> mailAccountCreatedBySigningContractReminder
                      Just (Signable Order) -> mailAccountCreatedBySigningOrderReminder
                      t -> error $ "Something strange happened (document with a type " ++ show t ++ " was signed and now reminder wants to be sent)"
                templates <- getLangTemplates $ lang $ usersettings user
                mail <- liftIO $ runLocalTemplates templates $ mailfunc (hostpart $ sdAppConf sd) doctitle (getFullName user) (LinkAccountCreatedBySigning actionID token)
                scheduleEmailSendout (sdMailEnforcer sd) $ mail { to = [getMailAddress user]})
            _ <- update $ UpdateActionType actionID $ AccountCreatedBySigning {
                  acbsState = ReminderSent
                , acbsUserID = uid
                , acbsDocLinkDataID = doclinkdataid
                , acbsToken = token
            }
            _ <- update $ UpdateActionEvalTime actionID ((72 * 60) `minutesAfter` now)
            return ()

evaluateAction Action{actionID, actionType = EmailSendout mail@Mail{mailInfo}} =
 if (unsendable mail)
  then do -- Due to next block, bad emails were alive in queue, and making logs unreadable.
      Log.error $ "Unsendable email found: " ++ show mail
      _ <- update $ DeleteAction actionID
      Log.error $ "Email was removed from the queue"
  else do
    mailer <- sdMailer <$> ask
    success <- liftIO $ sendMail mailer actionID mail
    if success
       then do
           -- morph action type into SentEmailInfo
           let email' = email (head (to mail))
           now <- liftIO getMinutesTime
           _ <- update $ UpdateActionType actionID $ SentEmailInfo {
                 seiEmail            = Email email'
               , seiMailInfo         = mailInfo
               , seiEventType        = Other "passed to sendgrid"
               , seiLastModification = now
           }
           _ <- update $ UpdateActionEvalTime actionID $ (60*24*30) `minutesAfter` now
           return ()
       else do
           now <- liftIO $ getMinutesTime
           _ <- update $ UpdateActionEvalTime actionID $ 5 `minutesAfter` now
           return ()

evaluateAction Action{actionID, actionType = SentEmailInfo{}} = do
    deleteAction actionID

deleteAction :: ActionID -> ActionScheduler ()
deleteAction aid = do
    _ <- update $ DeleteAction aid
    return ()

getLangTemplates :: Lang -> ActionScheduler KontrakcjaTemplates
getLangTemplates lang = do
    sd <- ask
    (_, templates) <- liftIO $ second (langVersion lang) <$> readMVar (sdTemplates sd)
    return templates

-- Old scheduler internal stuff

timeoutDocuments :: MinutesTime -> ActionScheduler ()
timeoutDocuments now = do
    docs <- query $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do
        _ <- update $ TimeoutDocument (documentid doc) now
        Log.debug $ "Document timedout " ++ (show $ documenttitle doc)

