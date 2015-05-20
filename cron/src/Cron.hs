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

    withConsumer (documentAPICallback runScheduler) pool $ do
      withConsumer (cronQueue appConf mmixpanel templates runScheduler runDB) pool $ do
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
          return . RerunAfter $ ihours 1
        AmazonUpload -> do
          if AWS.isAWSConfigOk $ amazonConfig appConf
            then do
              moved <- runScheduler AWS.uploadSomeFileToAmazon
              if moved
                then return . RerunAfter $ iseconds 1
                else return . RerunAfter $ iminutes 1
            else do
              logInfo_ "AmazonUpload: no valid AWS config, skipping."
              return . RerunAfter $ iminutes 1
        AsyncEventsProcessing -> do
          runDB $ asyncProcessEvents (catEventProcs $ catMaybes [mmixpanel]) All
          return . RerunAfter $ iseconds 10
        ClockErrorCollection -> do
          runDB $ collectClockError (ntpServers appConf)
          return . RerunAfter $ ihours 1
        DocumentAutomaticRemindersEvaluation -> do
          runScheduler $ actionQueue documentAutomaticReminder
          return . RerunAfter $ iminutes 1
        DocumentsPurge -> do
          runScheduler $ do
            purgedCount <- dbUpdate $ PurgeDocuments 30 unsavedDocumentLingerDays
            logInfo_ $ "DocumentsPurge: purged" <+> show purgedCount <+> "documents."
          return . RerunAfter $ iminutes 10
        DocumentsArchiveIdle -> do
          runScheduler $ do
            now <- currentTime
            archived <- dbUpdate $ ArchiveIdleDocuments now
            logInfo_ $ "DocumentsArchiveIdle: archived documents for" <+> show archived <+> "signatories."
          return . RerunAfter $ ihours 24
        EmailChangeRequestsEvaluation -> do
          runScheduler $ actionQueue emailChangeRequest
          return . RerunAfter $ ihours 1
        FindAndDoPostDocumentClosedActions -> do
          runScheduler $ findAndDoPostDocumentClosedActions Nothing
          return . RerunAfter $ ihours 6
        FindAndDoPostDocumentClosedActionsNew -> do
          runScheduler $ findAndDoPostDocumentClosedActions (Just 6) -- hours
          return . RerunAfter $ iminutes 10
        FindAndExtendDigitalSignatures -> do
          runScheduler findAndExtendDigitalSignatures
          return . RerunAfter $ iminutes 30
        FindAndTimeoutDocuments -> do
          runScheduler findAndTimeoutDocuments
          return . RerunAfter $ iminutes 10
        MailEventsProcessing -> do
          runScheduler Mails.Events.processEvents
          return . RerunAfter $ iseconds 5
        OldDraftsRemoval -> do
          runScheduler $ do
            delCount <- dbUpdate $ RemoveOldDrafts 100
            logInfo_ $ "OldDraftsRemoval: removed" <+> show delCount <+> "old, unsaved draft documents."
          return . RerunAfter $ ihours 1
        PasswordRemindersEvaluation -> do
          runScheduler $ actionQueue passwordReminder
          return . RerunAfter $ ihours 1
        RecurlySynchronization -> do
          time <- runDB $ do
            time <- currentTime
            temps <- snd <$> readMVar templates
            handleSyncWithRecurly appConf (mailsConfig appConf)
              temps (recurlyAPIKey $ recurlyConfig appConf) time
            handleSyncNoProvider time
            return time
          -- retry the next day at midnight
          return $ RerunAt UTCTime {
            utctDay = 1 `addDays` utctDay time
          , utctDayTime = 0
          }
        SessionsEvaluation -> do
          runScheduler $ actionQueue session
          return . RerunAfter $ ihours 1
        SMSEventsProcessing -> do
          runScheduler SMS.Events.processEvents
          return . RerunAfter $ iseconds 5
        UserAccountRequestEvaluation -> do
          runScheduler $ actionQueue userAccountRequest
          return . RerunAfter $ ihours 1
      logInfo_ $ show cjType <+> "finished successfully."
      return $ Ok action
    , ccOnException = \_ CronJob{..} -> return $ case cjAttempts of
      1 -> RerunAfter $ iminutes 1
      2 -> RerunAfter $ iminutes 5
      3 -> RerunAfter $ iminutes 10
      4 -> RerunAfter $ iminutes 15
      5 -> RerunAfter $ iminutes 30
      _ -> RerunAfter $ ihours 1
    }
