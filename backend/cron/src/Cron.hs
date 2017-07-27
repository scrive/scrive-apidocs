module Cron (main) where

import Control.Monad
import Control.Monad.Base
import Crypto.RNG
import Data.Maybe
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Log
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Traversable as F

import ActionQueue.EmailChangeRequest
import ActionQueue.Monad
import ActionQueue.PasswordReminder
import ActionQueue.Scheduler
import ActionQueue.UserAccountRequest
import Administration.Invoicing
import AppDBTables
import Configuration
import Cron.Model
import CronConf
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import Doc.Action
import Doc.API.Callback.Model
import Doc.AutomaticReminder.Model
import Doc.Extending.Consumer
import Doc.Model
import Doc.Sealing.Consumer
import Doc.Signing.Consumer
import HostClock.Collector (collectClockError)
import KontraError
import KontraPrelude hiding (All)
import Log.Configuration
import Log.Identifier
import Log.Model
import Log.Utils
import Mails.Events
import MinutesTime
import Monitoring
import Planhat
import Purging.Files
import Session.Data
import SMS.Events
import Templates
import ThirdPartyStats.Core
import ThirdPartyStats.Mixpanel
import ThirdPartyStats.Planhat
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
    configFile = "cron.conf"

----------------------------------------

type CronM = CryptoRNGT (LogT IO)
type DBCronM = DBT CronM

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  cronConf <- readConfig putStrLn config
  case monitoringConf cronConf of
    Just conf -> void $ startMonitoringServer conf
    Nothing   -> return ()
  rng <- newCryptoRNGState

  logRunner <- mkLogRunner "cron" (logConfig cronConf) rng
  reqManager <- newTlsManager

  runWithLogRunner logRunner $ do
    checkExecutables

    let connSettings = pgConnSettings $ dbConfig cronConf
    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $
      checkDatabase kontraDomains kontraTables

    ConnectionSource pool <- ($ (maxConnectionTracker $ maxDBConnections cronConf))
      <$> liftBase (createPoolSource (connSettings kontraComposites) (maxDBConnections cronConf))
    templates <- liftBase readGlobalTemplates
    mrediscache <- F.forM (redisCacheConfig cronConf) mkRedisConnection
    filecache <- MemCache.new BS.length (localFileCacheSize cronConf)

    -- Asynchronous event dispatcher; if you want to add a consumer to the event
    -- dispatcher, please combine the two into one dispatcher function rather
    -- than creating a new thread or something like that, since
    -- asyncProcessEvents removes events after processing.
    mmixpanel <- case mixpanelToken cronConf of
      Nothing -> do
        noConfigurationWarning "Mixpanel"
        return Nothing
      Just mt ->
        return $ Just $ processMixpanelEvent mt

    mplanhat <- case planhatConf cronConf of
      Nothing -> do
        noConfigurationWarning "Planhat"
        return Nothing
      Just phConf ->
        return . Just $ processPlanhatEvent reqManager phConf

    let runDB :: DBCronM r -> CronM r
        runDB = withPostgreSQL pool

        runScheduler :: Scheduler r -> CronM r
        runScheduler = runDB . CronEnv.runScheduler cronConf
          filecache mrediscache templates

        docSealing   = documentSealing (amazonConfig cronConf)
          (guardTimeConf cronConf) templates filecache mrediscache pool (mailNoreplyAddress cronConf)
        docSigning   = documentSigning (amazonConfig cronConf)
          (guardTimeConf cronConf) (cgiGrpConfig cronConf)
          templates filecache mrediscache pool (mailNoreplyAddress cronConf)
        docExtending = documentExtendingConsumer (amazonConfig cronConf)
          (guardTimeConf cronConf) templates filecache mrediscache pool

        apiCallbacks = documentAPICallback runScheduler (mailNoreplyAddress cronConf)
        cron = cronQueue cronConf reqManager mmixpanel mplanhat runScheduler runDB

    runCryptoRNGT rng
      . finalize (localDomain "document sealing" $ runConsumer docSealing pool)
      . finalize (localDomain "document signing" $ runConsumer docSigning pool)
      . finalize (localDomain "document extending" $ runConsumer docExtending pool)
      . finalize (localDomain "api callbacks" $ runConsumer apiCallbacks pool)
      . finalize (localDomain "cron" $ runConsumer cron pool) $ do
      liftBase waitForTermination
  where
    cronQueue :: CronConf
              -> Manager
              -> Maybe (EventProcessor (DBCronM))
              -> Maybe (EventProcessor (DBCronM))
              -> (forall r. Scheduler r -> CronM r)
              -> (forall r. DBCronM r -> CronM r)
              -> ConsumerConfig CronM JobType CronJob
    cronQueue cronConf mgr mmixpanel mplanhat runScheduler runDB = ConsumerConfig {
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
          if AWS.isAWSConfigOk $ amazonConfig cronConf
            then do
              moved <- runScheduler (AWS.uploadSomeFilesToAmazon 10)
              if moved
                then return . RerunAfter $ iseconds 1
                else return . RerunAfter $ iminutes 1
            else do
              logInfo_ "No valid AWS config, skipping"
              return . RerunAfter $ iminutes 1
        AsyncEventsProcessing -> do
          runDB $ do
            let processMaximum = 200
            asyncProcessEvents getEventProcessor (NoMoreThan processMaximum)
          return . RerunAfter $ iseconds 10
        ClockErrorCollection -> do
          runDB $ collectClockError (ntpServers cronConf)
          return . RerunAfter $ ihours 1
        DocumentAutomaticRemindersEvaluation -> do
          runScheduler $ actionQueue documentAutomaticReminder
          return . RerunAfter $ iminutes 1
        DocumentsPurge -> do
          runScheduler $ do
            purgedCount <- dbUpdate . PurgeDocuments 30 $ fromIntegral unsavedDocumentLingerDays
            logInfo "Purged documents" $ object [
                "purged" .= purgedCount
              ]
          return . RerunAfter $ iminutes 10
        DocumentsArchiveIdle -> do
          runScheduler $ do
            now <- currentTime
            archived <- dbUpdate $ ArchiveIdleDocuments now
            logInfo "Archived documents for signatories" $ object [
                "signatory_count" .= archived
              ]
          RerunAt . nextDayAtHour 19 <$> currentTime
        EmailChangeRequestsEvaluation -> do
          runScheduler $ actionQueue emailChangeRequest
          return . RerunAfter $ ihours 1
        FindAndExtendDigitalSignatures -> do
          runScheduler findAndExtendDigitalSignatures
          return . RerunAfter $ iminutes 5
        FindAndTimeoutDocuments -> do
          runScheduler findAndTimeoutDocuments
          return . RerunAfter $ iminutes 10
        InvoicingUpload -> do
          case invoicingSFTPConf cronConf of
            Nothing -> do
              logInfo "SFTP config missing; skipping" $ object []
            Just sftpConfig -> runScheduler $ uploadInvoicing sftpConfig
          RerunAt . nextDayAtHour 1 <$> currentTime
        MailEventsProcessing -> do
          runScheduler $ Mails.Events.processEvents (mailNoreplyAddress cronConf)
          return . RerunAfter $ iseconds 5
        MarkOrphanFilesForPurge -> do
          let maxMarked = 100000
              -- Share the string between all the log messages.
              orphanFileMarked = "Orphan file marked for purge"
          fids <- runDB . dbUpdate . MarkOrphanFilesForPurgeAfter maxMarked $ idays 7
          forM_ fids $ \fid -> logInfo orphanFileMarked $ object [identifier_ fid]
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
          let connSource ci = simpleSource def { csConnInfo = ci }
              logDBs = catMaybes . for (lcLoggers $ logConfig cronConf) $ \case
                PostgreSQL ci -> Just ci
                _             -> Nothing
          forM_ logDBs $ \ci -> runDBT (unConnectionSource $ connSource ci) def $ do
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
        PushPlanhatStats -> do
          case planhatConf cronConf of
            Nothing -> do
              logInfo "Planhat config missing; skipping" $ object []
            Just phConf -> do runScheduler $ doDailyPlanhatStats phConf mgr
          RerunAt . nextDayAtHour 2 <$> currentTime
        SessionsEvaluation -> do
          runScheduler $ actionQueue session
          return . RerunAfter $ ihours 1
        SMSEventsProcessing -> do
          runScheduler $ SMS.Events.processEvents (mailNoreplyAddress cronConf)
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
        logHandlerInfo jobType action = localRandomID "job_id" $ localData ["job_type" .= show jobType] action

        getEventProcessor :: EventType -> Maybe (EventProcessor DBCronM)
        getEventProcessor EventMixpanel = mmixpanel
        getEventProcessor EventPlanhat  = mplanhat
