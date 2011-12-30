module ActionScheduler (
      ActionScheduler
    , runScheduler
    , runEnforceableScheduler
    , actionScheduler
    , oldScheduler
    , runDocumentProblemsCheck
    , runArchiveProblemsCheck
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Happstack.State
import Database.HDBC.PostgreSQL
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS hiding (length)

import AppControl (AppConf(..))
import ActionSchedulerState
import Archive.Invariants
import DB.Classes
import Doc.DocStateData
import Doc.Transitory
import KontraLink
import MinutesTime
import Mails.MailsData
import Mails.MailsConfig
import Mails.SendMail
import Session
import Templates.LocalTemplates
import Templates.Templates
import User.Model
import User.UserView
import qualified AppLogger as Log
import System.Time
import Util.HasSomeUserInfo
import Doc.Invariants
import Stats.Control

type SchedulerData' = SchedulerData AppConf (MVar (ClockTime, KontrakcjaGlobalTemplates)) MailsConfig

newtype ActionScheduler a = AS { unAS :: ReaderT SchedulerData' (ReaderT Connection IO) a }
    deriving (Monad, Functor, MonadIO, MonadReader SchedulerData')

instance DBMonad ActionScheduler where
    getConnection = AS $ lift ask
    handleDBError = E.throw

-- Note: Do not define TemplatesMonad instance for ActionScheduler, use
-- LocalTemplates instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

runScheduler :: ActionScheduler () -> SchedulerData' -> IO ()
runScheduler sched sd =
    withPostgreSQL (dbConfig $ sdAppConf sd) $ runReaderT (runReaderT (unAS sched) sd)

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
    conn <- getConnection
    let runAction a = runReaderT (runReaderT (unAS $ evaluateAction a) sd) conn `E.catch` catchEverything a
    liftIO $ getMinutesTime
         >>= query . GetExpiredActions imp
         >>= sequence_ . map runAction
    where
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
            mdoc <- doc_query $ GetDocumentByDocumentID docid
            let doctitle = maybe BS.empty documenttitle mdoc
            (runDBQuery $ GetUserByID uid) >>= maybe (return ()) (\user -> do
                let mailfunc :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
                    mailfunc = case documenttype <$> mdoc of
                      Just (Signable Offer) -> mailAccountCreatedBySigningOfferReminder
                      Just (Signable Contract) -> mailAccountCreatedBySigningContractReminder
                      Just (Signable Order) -> mailAccountCreatedBySigningOrderReminder
                      t -> error $ "Something strange happened (document with a type " ++ show t ++ " was signed and now reminder wants to be sent)"
                globaltemplates <- getGlobalTemplates
                mail <- liftIO $ runWithTemplates (getLocale user) globaltemplates $
                          mailfunc (hostpart $ sdAppConf sd) doctitle (getFullName user) (LinkAccountCreatedBySigning actionID token)
                scheduleEmailSendout (sdMailsConfig sd) $ mail { to = [getMailAddress user]})
            _ <- update $ UpdateActionType actionID $ AccountCreatedBySigning {
                  acbsState = ReminderSent
                , acbsUserID = uid
                , acbsDocLinkDataID = doclinkdataid
                , acbsToken = token
            }
            _ <- update $ UpdateActionEvalTime actionID ((72 * 60) `minutesAfter` now)
            return ()

evaluateAction Action{actionID, actionType = RequestEmailChange{}} =
  deleteAction actionID

evaluateAction Action{actionType = EmailSendout{}} =
  error "Sending emails was moved out of kontrakcja"

{-
evaluateAction Action{actionID, actionType = EmailSendout mail@Mail{mailInfo}} =
 if (unsendable mail)
  then do -- Due to next block, bad emails were alive in queue, and making logs unreadable.
      Log.error $ "Unsendable email found: " ++ show mail
      _ <- update $ DeleteAction actionID
      Log.error $ "Email was removed from the queue"
  else do
    mailer <- sdMailer <$> ask
    appconf <- sdAppConf <$> ask
    let backdooropen = isBackdoorOpen $ mailsConfig appconf
    success <- sendMail mailer actionID mail
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
               , seiBackdoorInfo     = if backdooropen
                                         then Just $ ActionBackdoorInfo { bdContent = content mail }
                                         else Nothing
           }
           _ <- update $ UpdateActionEvalTime actionID $ (60*24*30) `minutesAfter` now
           return ()
       else do
           now <- liftIO $ getMinutesTime
           _ <- update $ UpdateActionEvalTime actionID $ 5 `minutesAfter` now
           return ()
-}
evaluateAction Action{actionID, actionType = SentEmailInfo{}} = do
    deleteAction actionID

runDocumentProblemsCheck :: ActionScheduler ()
runDocumentProblemsCheck = do
  sd <- ask
  now <- liftIO getMinutesTime
  docs <- doc_query $ GetDocuments Nothing
  let probs = listInvariantProblems now docs
  when (probs /= []) $ mailDocumentProblemsCheck $
    "<p>"  ++ (hostpart $ sdAppConf sd) ++ "/dave/document/" ++
    intercalate ("</p>\n\n<p>" ++ (hostpart $ sdAppConf sd) ++ "/dave/document/") probs ++
    "</p>"
  return ()

-- | Send an email out to all registered emails about document problems.
mailDocumentProblemsCheck :: String -> ActionScheduler ()
mailDocumentProblemsCheck msg = do
  sd <- ask
  scheduleEmailSendout (sdMailsConfig sd) $ Mail { to = zipWith MailAddress documentProblemsCheckEmails documentProblemsCheckEmails
                                                  , title = "Document problems report " ++ (hostpart $ sdAppConf sd)
                                                  , content = msg
                                                  , attachments = []
                                                  , from = Nothing
                                                  , mailInfo = None
                                                  }

-- | A message will be sent to these email addresses when there is an inconsistent document found in the database.
documentProblemsCheckEmails :: [BS.ByteString]
documentProblemsCheckEmails = map BS.fromString ["bugs@skrivapa.se"]

runArchiveProblemsCheck :: ActionScheduler ()
runArchiveProblemsCheck = do
  users <- runDBQuery $ GetUsers
  personaldocs <- mapM getPersonalDocs users
  superviseddocs <- mapM getSupervisedDocs users
  let personaldocprobs = listPersonalDocInvariantProblems personaldocs
      supervisedocprobs = listSupervisedDocInvariantProblems superviseddocs
      probs = unlines personaldocprobs ++ unlines supervisedocprobs
  when (probs /= []) $ mailArchiveProblemsCheck probs
  return ()
  where
    getPersonalDocs user = do
      docs <- doc_query $ GetDocumentsBySignatory user
      return (user, docs)
    getSupervisedDocs user = do
      docs <- doc_query $ GetDocumentsByCompany user
      return (user, docs)

mailArchiveProblemsCheck :: String -> ActionScheduler ()
mailArchiveProblemsCheck msg = do
  sd <- ask
  scheduleEmailSendout (sdMailsConfig sd) $ Mail { to = zipWith MailAddress archiveProblemsCheckEmails archiveProblemsCheckEmails
                                                  , title = "Archive problems report " ++ (hostpart $ sdAppConf sd)
                                                  , content = msg
                                                  , attachments = []
                                                  , from = Nothing
                                                  , mailInfo = None
                                                  }
archiveProblemsCheckEmails :: [BS.ByteString]
archiveProblemsCheckEmails = map BS.fromString ["emily@scrive.com"]

deleteAction :: ActionID -> ActionScheduler ()
deleteAction aid = do
    _ <- update $ DeleteAction aid
    return ()

getGlobalTemplates :: ActionScheduler KontrakcjaGlobalTemplates
getGlobalTemplates = do
    sd <- ask
    (_, templates) <- liftIO $ readMVar (sdTemplates sd)
    return templates

-- Old scheduler internal stuff

timeoutDocuments :: MinutesTime -> ActionScheduler ()
timeoutDocuments now = do
    docs <- doc_query $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do
        edoc <- doc_update $ TimeoutDocument (documentid doc) now
        case edoc of
          Left _ -> return ()
          Right doc' -> do
            _ <- addDocumentTimeoutStatEvents doc'
            return ()
        Log.debug $ "Document timedout " ++ (show $ documenttitle doc)

