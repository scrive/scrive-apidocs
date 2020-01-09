module MailingServer (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Crypto.RNG
import Data.Aeson
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Happstack.Server hiding (result, waitForTermination)
import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.FilePath ((</>), FilePath)
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as F
import qualified Happstack.StaticRouting as R

import AppDir (AppPaths(..), setupAppPaths)
import Configuration
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import FileStorage
import FileStorage.Amazon.S3Env
import Handlers
import Happstack.Server.ReqHandler
import Log.Configuration
import Log.Identifier
import Log.Utils
import MailingServerConf
import Mails.KontraInfoForMail
import Mails.Model
import Mails.Tables
import MinutesTime
import Monitoring
import Sender
import Utils.IO
import Utils.Network

data CmdConf = CmdConf
  { config :: String
  } deriving (Data, Typeable)

cmdConf :: FilePath -> String -> CmdConf
cmdConf workspaceRoot progName =
  CmdConf
      { config =
        configFile &= help ("Configuration file (default: " ++ configFile ++ ")") &= typ
          "FILE"
      }
    &= program progName
  where configFile = workspaceRoot </> "mailing_server.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = do
  (AppPaths _ workspaceRoot) <- setupAppPaths
  -- All running instances need to have the same configuration.
  CmdConf {..}               <- cmdArgs . cmdConf workspaceRoot =<< getProgName
  conf                       <- readConfig putStrLn config
  case mailerMonitoringConfig conf of
    Just mconf -> void $ startMonitoringServer mconf
    Nothing    -> return ()
  rng        <- newCryptoRNGState
  (errs, lr) <- mkLogRunner "mailer" (mailerLogConfig conf) rng
  mapM_ T.putStrLn errs

  withLogger lr $ \runLogger -> runLogger $ do
    checkExecutables

    let pgSettings    = pgConnSettings $ mailerDBConfig conf
        extrasOptions = defaultExtrasOptions
    withPostgreSQL (unConnectionSource . simpleSource $ pgSettings []) $ do
      checkDatabaseAllowUnknownObjects extrasOptions [] [] mailerTables
    fsConf <- do
      env         <- s3envFromConfig $ mailerAmazonConfig conf
      localCache  <- newFileMemCache $ mailerLocalFileCacheSize conf
      globalCache <- F.forM (mailerRedisCacheConfig conf) mkRedisConnection
      return (env, globalCache, localCache)
    cs@(ConnectionSource pool) <-
      ($ (maxConnectionTracker $ mailerMaxDBConnections conf)) <$> liftBase
        (createPoolSource (pgSettings mailerComposites) (mailerMaxDBConnections conf))

    E.bracket (startServer runLogger conf cs rng) (liftBase . killThread) . const $ do
      let master = createSender cs $ mailerMasterSender conf
          mslave = createSender cs <$> mailerSlaveSender conf
          cron   = jobsWorker conf cs rng
          sender = mailsConsumer fsConf master mslave cs rng

      finalize (localDomain "cron" $ runConsumer cron pool) $ do
        finalize (localDomain "sender" $ runConsumer sender pool) $ do
          liftBase waitForTermination
  where
    startServer
      :: (forall m r. Monad m => LogT m r -> m r)
      -> MailingServerConf
      -> TrackedConnectionSource
      -> CryptoRNGState
      -> MainM ThreadId
    startServer runLogger conf cs rng = do
      let (iface, port) = mailerHttpBindAddress conf
          handlerConf   = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile (handlers conf) of
        Left e -> do
          logInfo "Error while compiling routes" $ object ["error" .= e]
          unexpectedError "static routing"
        Right r ->
          return
            $   r
            >>= maybe
                  (  logInfo_ "Not found"
                  >> (notFound $ toResponse ("Not found." :: String))
                  )
                  return
      socket <- liftBase . listenOn (htonl iface) $ fromIntegral port
      fork . liftBase . runReqHandlerT socket handlerConf . runLogger $ router rng
                                                                               cs
                                                                               routes

    hasFailoverTests conf = case mailerSlaveSender conf of
      Just _  -> not $ null $ mailerTestReceivers conf
      Nothing -> False

    mailsConsumer
      :: FileStorageConfig
      -> Sender
      -> Maybe Sender
      -> TrackedConnectionSource
      -> CryptoRNGState
      -> ConsumerConfig MainM MailID Mail
    mailsConsumer fsConf master mslave (ConnectionSource pool) rng = ConsumerConfig
      { ccJobsTable           = "mails"
      , ccConsumersTable      = "mailer_workers"
      , ccJobSelectors        = mailSelectors
      , ccJobFetcher          = mailFetcher
      , ccJobIndex            = mailID
      , ccNotificationChannel = Just mailNotificationChannel
      , ccNotificationTimeout = 60 * 1000000 -- 1 minute
      , ccMaxRunningJobs      = 10
      , ccProcessJob          =
        \mail@Mail {..} -> localData [identifier mailID] $ if isNotSendable mail
          then do
            logInfo "Email is not sendable, discarding" $ object ["mail" .= show mail]
            return $ Failed Remove
          else runCryptoRNGT rng . runFileStorageT fsConf $ do
            senderType <- if mailServiceTest
              then return MasterSender
              else withPostgreSQL pool $ dbQuery GetCurrentSenderType
            sender <- case senderType of
              MasterSender -> return master
              SlaveSender  -> case mslave of
                Just slave -> return slave
                Nothing    -> do
                  logAttention_ "No slave sender, falling back to master"
                  return master
            logInfo "Sending email" $ object ["sender" .= show sender]
            sendMail sender mail >>= \case
              True  -> return $ Ok MarkProcessed
              False -> Failed <$> sendoutFailed mail
      , ccOnException         = const sendoutFailed
      }
      where
        isNotSendable Mail {..} =
          T.null (addrEmail mailFrom) || null mailTo || any (T.null . addrEmail) mailTo

        sendoutFailed Mail {..} = do
          logInfo_ "Failed to send email"
          if mailAttempts < 100
            then do
              logInfo_ "Deferring email for 5 minutes"
              return . RerunAfter $ iminutes 5
            else do
              logInfo_
                "Deleting email since there were 100 unsuccessful attempts to send it"
              return Remove

    jobsWorker
      :: MailingServerConf
      -> TrackedConnectionSource
      -> CryptoRNGState
      -> ConsumerConfig MainM JobType MailerJob
    jobsWorker conf (ConnectionSource pool) rng = ConsumerConfig
      { ccJobsTable           = "mailer_jobs"
      , ccConsumersTable      = "mailer_workers"
      , ccJobSelectors        = mailerJobSelectors
      , ccJobFetcher          = mailerJobFetcher
      , ccJobIndex            = mjType
      , ccNotificationChannel = Nothing
      , ccNotificationTimeout = 60 * 1000000 -- 1 minute
      , ccMaxRunningJobs      = 1
      , ccProcessJob          =
        \MailerJob {..} -> runCryptoRNGT rng . logHandlerInfo mjType $ case mjType of
          CleanOldEmails -> do
            let daylimit = 14
            logInfo_ $ "Removing emails sent" <+> T.pack (show daylimit) <+> "days ago."
            cleaned <- withPostgreSQL pool . dbUpdate $ CleanEmailsOlderThanDays daylimit
            logInfo "Old emails removed" $ object ["removed" .= cleaned]
            Ok . RerunAt . nextDayMidnight <$> currentTime

          PerformServiceTest -> withPostgreSQL pool $ if hasFailoverTests conf
            then runCryptoRNGT rng $ do
              -- If there is no slave sender/test receivers, retry periodically to
              -- be able to start the process if the configuration changes.
              logInfo_ "Running service checker"
              token <- random
              mid   <- dbUpdate $ CreateServiceTest
                ( token
                , testSender
                , mailerTestReceivers conf
                , Just testSender
                , "test"
                , "test"
                , []
                )
              logInfo "Service testing email created" $ object [identifier mid]
              dbUpdate $ CollectServiceTestResultIn $ iminutes 10
              return $ Ok MarkProcessed
            else do
              dbUpdate $ CollectServiceTestResultIn $ iseconds 50
              return $ Ok MarkProcessed
          CollectServiceTestResult -> withPostgreSQL pool $ if hasFailoverTests conf
            then runCryptoRNGT rng $ do
              events <- dbQuery GetServiceTestEvents
              result <- if any isDelivered events
                then do
                  logInfo_ "Service testing emails were delivered successfully"
                  return Ok
                else do
                  logInfo_
                    "Service testing emails failed to be delivered within 10 minutes"
                  sender <- dbQuery GetCurrentSenderType
                  when (sender == MasterSender) $ do
                    logInfo_
                      "Switching to slave sender and resending all emails that were sent within this time"
                    dbUpdate SwitchToSlaveSenderImmediately
                    resent <- dbUpdate ResendEmailsSentAfterServiceTest
                    logInfo "Emails set to be resent" $ object ["emails" .= resent]
                  return Failed
              dbUpdate ScheduleServiceTest
              return $ result MarkProcessed
            else do
              dbUpdate ScheduleServiceTest
              return $ Ok MarkProcessed
      , ccOnException         = \_ _ -> return . RerunAfter $ ihours 1
      }
      where
        logHandlerInfo jobType action =
          localRandomID "job_id" $ localData ["job_type" .= show jobType] action

        isDelivered (_, _, SendGridEvent _ SG_Delivered{} _, _) = True
        isDelivered (_, _, MailGunEvent _ MG_Delivered, _) = True
        isDelivered (_, _, SocketLabsEvent _ SL_Delivered, _) = True
        isDelivered _ = False

        testSender =
          Address { addrName = "Scrive mailer", addrEmail = "noreply@scrive.com" }
