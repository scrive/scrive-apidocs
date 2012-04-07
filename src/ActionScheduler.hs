module ActionScheduler (
      ActionScheduler
    , runScheduler
    , runEnforceableScheduler
    , actionScheduler
    , oldScheduler
    , runDocumentProblemsCheck
    , runArchiveProblemsCheck
    , getGlobalTemplates
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Happstack.State
import qualified Control.Exception as E

import AppControl (AppConf(..))
import ActionSchedulerState
import Crypto.RNG (CryptoRNG, getCryptoRNGState, CryptoRNGState)
import DB.Classes
import Doc.DocStateData
import Doc.Model
import MinutesTime
import Mails.MailsData
import Mails.MailsConfig
import Mails.SendMail
import Session
import Templates.Templates
import qualified Log
import System.Time
import Doc.Invariants
import Stats.Control
import EvidenceLog.Model

type SchedulerData' = SchedulerData AppConf (MVar (ClockTime, KontrakcjaGlobalTemplates)) MailsConfig

newtype ActionScheduler a = AS { unAS :: ReaderT SchedulerData' (ReaderT DBEnv IO) a }
    deriving (Monad, Functor, MonadIO, MonadReader SchedulerData')

instance CryptoRNG ActionScheduler where
  getCryptoRNGState = AS $ lift $ asks envRNG

instance DBMonad ActionScheduler where
    getDBEnv = AS $ lift ask
    handleDBError = E.throw

-- Note: Do not define TemplatesMonad instance for ActionScheduler, use
-- LocalTemplates instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

runScheduler :: CryptoRNGState -> ActionScheduler () -> SchedulerData' -> IO ()
runScheduler rng sched sd =
    withPostgreSQLDB' (dbConfig $ sdAppConf sd) rng $
      runReaderT (runReaderT (unAS sched) sd)

-- | Creates scheduler that may be forced to look up for actions to execute
runEnforceableScheduler :: CryptoRNGState -> Int -> MVar () -> ActionScheduler () -> SchedulerData' -> IO ()
runEnforceableScheduler rng interval enforcer sched sd = listen 0
    where
        listen delay = do
            run_now <- tryTakeMVar enforcer
            if isJust run_now || delay >= interval
               then runScheduler rng sched sd >> listen 0
               else threadDelay 1000000 >> (listen $! delay+1)

-- | Gets 'expired' actions and evaluates them
actionScheduler :: ActionImportance -> ActionScheduler ()
actionScheduler imp = do
    sd <- ask
    env <- getDBEnv
    let runAction a = runReaderT (runReaderT (unAS $ evaluateAction a) sd) env `E.catch` catchEverything a
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
evaluateAction Action{actionID, actionType = PasswordReminder{}} =
    deleteAction actionID

evaluateAction Action{actionID, actionType = ViralInvitationSent{}} =
    deleteAction actionID

evaluateAction Action{actionID, actionType = AccountCreated{}} =
    deleteAction actionID

evaluateAction Action{actionID, actionType = AccountCreatedBySigning{}} = do
  -- we used to send a "You haven't secured your original" email,
  -- but we don't anymore, so this just deletes the action
  deleteAction actionID

evaluateAction Action{actionID, actionType = RequestEmailChange{}} =
  deleteAction actionID

evaluateAction Action{actionID, actionType = DummyActionType} =
  deleteAction actionID

runDocumentProblemsCheck :: ActionScheduler ()
runDocumentProblemsCheck = do
  sd <- ask
  now <- liftIO getMinutesTime
  docs <- runDBQuery $ GetDocumentsByService Nothing
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
documentProblemsCheckEmails :: [String]
documentProblemsCheckEmails = ["bugs@skrivapa.se"]

runArchiveProblemsCheck :: ActionScheduler ()
runArchiveProblemsCheck = do
  return ()

{-

  This requires reorganization as there is no difference between personal and company documents now.

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
      docs <- runDBQuery $ GetDocumentsBySignatory [Contract, Offer, Order] user
      return (user, docs)
    getSupervisedDocs user = do
      docs <- runDBQuery $ GetDocumentsByCompany user
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

-}

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
    docs <- runDBQuery $ GetTimeoutedButPendingDocuments now
    forM_ docs $ \doc -> do
        edoc <- runDBUpdate $ TimeoutDocument (documentid doc) (SystemActor now)
        case edoc of
          Left _ -> return ()
          Right doc' -> do
            _ <- addDocumentTimeoutStatEvents doc'
            return ()
        Log.debug $ "Document timedout " ++ (show $ documenttitle doc)

