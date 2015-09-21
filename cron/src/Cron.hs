module Cron (main) where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Consumers
import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
import KontraPrelude hiding (All)
import Log.Configuration
import Log.Identifier
import Log.Model
import Log.Utils
import Mails.Events
import MinutesTime
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

data CmdConf = CmdConf {
  config :: String
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE"
} &= program progName
  where
    configFile = "kontrakcja.conf"

----------------------------------------

type CronM = CryptoRNGT (LogT IO)
type DBCronM = DBT CronM

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  appConf <- readConfig putStrLn config
  LogRunner{..} <- mkLogRunner "cron" $ logConfig appConf
  withLoggerWait $ do
    checkExecutables

    let connSettings = pgConnSettings $ dbConfig appConf
    withPostgreSQL (simpleSource $ connSettings []) $
      checkDatabase (logInfo_ . T.pack) kontraDomains kontraTables

    pool <- liftBase . createPoolSource (liftBase . withLogger . logAttention_ . T.pack) $ connSettings kontraComposites
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
        runDB = withPostgreSQL pool

        runScheduler :: Scheduler r -> CronM r
        runScheduler = runDB . CronEnv.runScheduler appConf filecache templates

        apiCallbacks = documentAPICallback runScheduler
        cron = cronQueue appConf mmixpanel templates runScheduler runDB

    runCryptoRNGT rng $ do
      finalize (localDomain "api callbacks" $ runConsumer apiCallbacks pool) $ do
        finalize (localDomain "cron" $ runConsumer cron pool) $ do
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
    , ccProcessJob = \CronJob{..} -> logHandlerInfo cjType $ do
      logInfo_ "Processing job"
      action <- case cjType of
        AmazonUpload -> do
          if AWS.isAWSConfigOk $ amazonConfig appConf
            then do
              moved <- runScheduler AWS.uploadSomeFileToAmazon
              if moved
                then return . RerunAfter $ iseconds 1
                else return . RerunAfter $ iminutes 1
            else do
              logInfo_ "No valid AWS config, skipping"
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
            logInfo "Purged documents" $ object [
                "purged" .= purgedCount
              ]
          return . RerunAfter $ iminutes 10
        DocumentsArchiveIdle -> do
          runScheduler $ do
            now <- currentTime
            archived <- dbUpdate $ ArchiveIdleDocuments now
            logInfo "Archived documents for signatories" $ object [
                "signatories" .= archived
              ]
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
        MarkOrphanFilesForPurge -> do
          let maxMarked = 1000
          fids <- runDB . dbUpdate . MarkOrphanFilesForPurgeAfter maxMarked $ idays 7
          logInfo "Orphan files marked for purge" $ object [
              identifiers fids
            ]
          -- If maximum amount of files was marked, run it again shortly after.
          if length fids == maxMarked
            then return . RerunAfter $ iseconds 1
            else RerunAt . nextDayMidnight <$> currentTime
        OldDraftsRemoval -> do
          runScheduler $ do
            delCount <- dbUpdate $ RemoveOldDrafts 100
            logInfo "Removed old, unsaved draft documents" $ object [
                "removed" .= delCount
              ]
          return . RerunAfter $ ihours 1
        OldLogsRemoval -> do
          let connSource ci = simpleSource def { csConnInfo = T.encodeUtf8 ci }
              logDBs = catMaybes . for (lcLoggers $ logConfig appConf) $ \case
                PostgreSQL ci -> Just ci
                _             -> Nothing
          forM_ logDBs $ \ci -> runDBT (connSource ci) def $ do
            runSQL_ "SELECT current_database()::text"
            dbName :: T.Text <- fetchOne runIdentity
            n <- dbUpdate $ CleanLogsOlderThanDays 30
            logInfo "Old logs removed" $ object [
                "database" .= dbName
              , "logs_removed" .= n
              ]
          RerunAt . nextDayMidnight <$> currentTime
        PasswordRemindersEvaluation -> do
          runScheduler $ actionQueue passwordReminder
          return . RerunAfter $ ihours 1
        PurgeOrphanFile -> do
          found <- runScheduler purgeOrphanFile
          return . RerunAfter $ if found
                                then iseconds 1
                                else iminutes 1
        RecurlySynchronization -> do
          time <- runDB $ do
            time <- currentTime
            temps <- snd <$> readMVar templates
            handleSyncWithRecurly appConf (mailsConfig appConf)
              temps (recurlyAPIKey $ recurlyConfig appConf) time
            handleSyncNoProvider time
            return time
          return . RerunAt $ nextDayMidnight time
        SessionsEvaluation -> do
          runScheduler $ actionQueue session
          return . RerunAfter $ ihours 1
        SMSEventsProcessing -> do
          runScheduler SMS.Events.processEvents
          return . RerunAfter $ iseconds 5
        UserAccountRequestEvaluation -> do
          runScheduler $ actionQueue userAccountRequest
          return . RerunAfter $ ihours 1
      logInfo_ "Job processed successfully"
      return $ Ok action
    , ccOnException = \_ CronJob{..} -> return $ case cjAttempts of
      1 -> RerunAfter $ iminutes 1
      2 -> RerunAfter $ iminutes 5
      3 -> RerunAfter $ iminutes 10
      4 -> RerunAfter $ iminutes 15
      5 -> RerunAfter $ iminutes 30
      _ -> RerunAfter $ ihours 1
    }
      where
        logHandlerInfo jobType = localRandomID "job_id" . localData ["job_type" .= show jobType]
