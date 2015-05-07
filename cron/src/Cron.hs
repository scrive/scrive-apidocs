module Cron where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Data.Maybe
import Data.Monoid.Utils
import Data.Time
import Log
import qualified Data.ByteString as BS

import ActionQueue.EmailChangeRequest
import ActionQueue.Monad
import ActionQueue.PasswordReminder
import ActionQueue.Scheduler
import ActionQueue.UserAccountRequest
import AppConf
import AppDBTables
import Configuration
import Cron.Model
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Doc.Action
import Doc.API.Callback.Model
import Doc.AutomaticReminder.Model
import Doc.Model
import HostClock.Collector (collectClockError)
import JobQueue.Components
import JobQueue.Config
import KontraPrelude hiding (All)
import Log.Configuration
import Mails.Events
import Payments.Config
import Payments.Control
import Purging.Files
import Session.Data
import SMS.Events
import Templates
import ThirdPartyStats.Core
import ThirdPartyStats.Mixpanel
import Utils.IO
import qualified Amazon as AWS
import qualified CronEnv
import qualified MemCache

type CronM = LogT IO
type DBCronM = CryptoRNGT (DBT CronM)

main :: IO ()
main = do
  appConf <- readConfig putStrLn "kontrakcja.conf"
  LogRunner{..} <- mkLogRunner "cron" $ logConfig appConf
  withLoggerWait $ do
    checkExecutables

    let connSettings = pgConnSettings $ dbConfig appConf
    withPostgreSQL (simpleSource $ connSettings []) $
      checkDatabase logInfo_ kontraDomains kontraTables

    pool <- liftBase . createPoolSource (liftBase . withLogger . logAttention_) $ connSettings kontraComposites
    templates <- liftBase (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
    rng <- newCryptoRNGState
    filecache <- MemCache.new BS.length 52428800

    -- Asynchronous event dispatcher; if you want to add a consumer to the event
    -- dispatcher, please combine the two into one dispatcher function rather
    -- than creating a new thread or something like that, since
    -- asyncProcessEvents removes events after processing.
    mmixpanel <- case mixpanelToken appConf of
      ""    -> logInfo_ "WARNING: no Mixpanel token present!" >> return Nothing
      token -> return $ Just $ processMixpanelEvent token

    let runDB :: DBCronM r -> CronM r
        runDB = withPostgreSQL pool . runCryptoRNGT rng

        runScheduler :: Scheduler r -> CronM r
        runScheduler = runDB . CronEnv.runScheduler appConf filecache templates

        cronLogger domain msg = logInfo_ $ "Cron:" <+> domain <> ":" <+> msg
        apiCallbackLogger domain msg = logInfo_ $ "API Callbacks:" <+> domain <> ":" <+> msg

    withConsumer (documentAPICallback runScheduler) pool apiCallbackLogger $ do
      withConsumer (cronQueue appConf mmixpanel templates runScheduler runDB) pool cronLogger $ do
        liftBase waitForTermination
  where
    cronQueue :: AppConf
              -> Maybe (EventProcessor (DBCronM))
              -> MVar (UTCTime, KontrakcjaGlobalTemplates)
              -> (forall r. Scheduler r -> CronM r)
              -> (forall r. DBCronM r -> CronM r)
              -> ConsumerConfig CronM JobType CronJob
    cronQueue appConf mmixpanel templates runScheduler runDB = ConsumerConfig {
      ccJobsTable = "cron_jobs"
    , ccConsumersTable = "cron_workers"
    , ccJobSelectors = cronJobSelectors
    , ccJobFetcher = cronJobFetcher
    , ccJobIndex = cjType
    , ccNotificationChannel = Nothing
    , ccNotificationTimeout = 3 * 1000000
    , ccMaxRunningJobs = 10
    , ccProcessJob = \CronJob{..} -> do
      logInfo_ $ "Processing" <+> show cjType <> "..."
      action <- case cjType of
        AmazonDeletion -> do -- This one should be renamed to FilesPurge
          runScheduler purgeSomeFiles
          return . RetryAfter $ ihours 1
        AmazonUpload -> do
          if AWS.isAWSConfigOk $ amazonConfig appConf
            then do
              moved <- runScheduler AWS.uploadSomeFileToAmazon
              if moved
                then return . RetryAfter $ iseconds 1
                else return . RetryAfter $ iminutes 1
            else do
              logInfo_ "AmazonUpload: no valid AWS config, skipping."
              return . RetryAfter $ iminutes 1
        AsyncEventsProcessing -> do
          runDB $ asyncProcessEvents (catEventProcs $ catMaybes [mmixpanel]) All
          return . RetryAfter $ iseconds 10
        ClockErrorCollection -> do
          runDB $ collectClockError (ntpServers appConf)
          return . RetryAfter $ ihours 1
        DocumentAutomaticRemindersEvaluation -> do
          runScheduler $ actionQueue documentAutomaticReminder
          return . RetryAfter $ iminutes 1
        DocumentsPurge -> do
          runScheduler $ do
            purgedCount <- dbUpdate $ PurgeDocuments 30 unsavedDocumentLingerDays
            logInfo_ $ "DocumentsPurge: purged" <+> show purgedCount <+> "documents."
          return . RetryAfter $ iminutes 10
        DocumentsArchiveIdle -> do
          runScheduler $ do
            now <- currentTime
            archived <- dbUpdate $ ArchiveIdleDocuments now
            logInfo_ $ "DocumentsArchiveIdle: archived documents for" <+> show archived <+> "signatories."
          return . RetryAfter $ ihours 24
        EmailChangeRequestsEvaluation -> do
          runScheduler $ actionQueue emailChangeRequest
          return . RetryAfter $ ihours 1
        FindAndDoPostDocumentClosedActions -> do
          runScheduler $ findAndDoPostDocumentClosedActions Nothing
          return . RetryAfter $ ihours 6
        FindAndDoPostDocumentClosedActionsNew -> do
          runScheduler $ findAndDoPostDocumentClosedActions (Just 6) -- hours
          return . RetryAfter $ iminutes 10
        FindAndExtendDigitalSignatures -> do
          runScheduler findAndExtendDigitalSignatures
          return . RetryAfter $ iminutes 30
        FindAndTimeoutDocuments -> do
          runScheduler findAndTimeoutDocuments
          return . RetryAfter $ iminutes 10
        MailEventsProcessing -> do
          runScheduler Mails.Events.processEvents
          return . RetryAfter $ iseconds 5
        OldDraftsRemoval -> do
          runScheduler $ do
            delCount <- dbUpdate $ RemoveOldDrafts 100
            logInfo_ $ "OldDraftsRemoval: removed" <+> show delCount <+> "old, unsaved draft documents."
          return . RetryAfter $ ihours 1
        PasswordRemindersEvaluation -> do
          runScheduler $ actionQueue passwordReminder
          return . RetryAfter $ ihours 1
        RecurlySynchronization -> do
          time <- runDB $ do
            time <- currentTime
            temps <- snd <$> readMVar templates
            handleSyncWithRecurly appConf (mailsConfig appConf)
              temps (recurlyAPIKey $ recurlyConfig appConf) time
            handleSyncNoProvider time
            return time
          -- retry the next day at midnight
          return $ RetryAt UTCTime {
            utctDay = 1 `addDays` utctDay time
          , utctDayTime = 0
          }
        SessionsEvaluation -> do
          runScheduler $ actionQueue session
          return . RetryAfter $ ihours 1
        SMSEventsProcessing -> do
          runScheduler SMS.Events.processEvents
          return . RetryAfter $ iseconds 5
        UserAccountRequestEvaluation -> do
          runScheduler $ actionQueue userAccountRequest
          return . RetryAfter $ ihours 1
      logInfo_ $ show cjType <+> "finished successfully."
      return $ Ok action
    , ccOnException = \CronJob{..} -> return $ case cjAttempts of
      1 -> RetryAfter $ iminutes 1
      2 -> RetryAfter $ iminutes 5
      3 -> RetryAfter $ iminutes 10
      4 -> RetryAfter $ iminutes 15
      5 -> RetryAfter $ iminutes 30
      _ -> RetryAfter $ ihours 1
    }
