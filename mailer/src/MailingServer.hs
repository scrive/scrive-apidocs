module MailingServer (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Data.Aeson
import Data.Monoid
import Database.PostgreSQL.Consumers
import Happstack.Server hiding (result, waitForTermination)
import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Happstack.StaticRouting as R

import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Handlers
import Happstack.Server.ReqHandler
import KontraPrelude
import Log.Configuration
import Log.Identifier
import Log.Utils
import MailingServerConf
import Mails.Model
import Mails.Tables
import MinutesTime
import Sender
import Utils.IO
import Utils.Network
import qualified Amazon as AWS
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
    configFile = "mailing_server.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = do
  -- All running instances need to have the same configuration.
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  conf <- readConfig putStrLn config
  lr@LogRunner{..} <- mkLogRunner "mailer" $ mscLogConfig conf
  withLoggerWait $ do
    checkExecutables

    let cs = pgConnSettings $ mscDBConfig conf
    withPostgreSQL (simpleSource $ cs []) $ do
      checkDatabase (logInfo_ . T.pack) [] mailerTables
    awsconf <- AWS.AmazonConfig (mscAmazonConfig conf) <$> MemCache.new BS.length 52428800
    pool <- liftBase . createPoolSource (liftBase . withLogger . logAttention_ . T.pack) $ cs mailerComposites
    rng <- newCryptoRNGState

    E.bracket (startServer lr conf pool rng) (liftBase . killThread) . const $ do
      let master = createSender pool $ mscMasterSender conf
          mslave = createSender pool <$> mscSlaveSender conf
          cron   = jobsWorker conf pool rng
          sender = mailsConsumer awsconf master mslave pool rng

      finalize (localDomain "cron" $ runConsumer cron pool) $ do
        finalize (localDomain "sender" $ runConsumer sender pool) $ do
          liftBase waitForTermination
  where
    startServer :: LogRunner -> MailingServerConf
                -> ConnectionSource -> CryptoRNGState -> MainM ThreadId
    startServer LogRunner{..} conf pool rng = do
      let (iface, port) = mscHttpBindAddress conf
          handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          logInfo "Error while compiling routes" $ object [
              "error" .= e
            ]
          $unexpectedErrorM "static routing"
        Right r -> return $ r >>= maybe (notFound $ toResponse ("Not found."::String)) return
      socket <- liftBase . listenOn (htonl iface) $ fromIntegral port
      fork . liftBase . runReqHandlerT socket handlerConf . mapReqHandlerT (withLogger) $ router rng pool routes

    mailsConsumer :: AWS.AmazonConfig -> Sender -> Maybe Sender -> ConnectionSource
                  -> CryptoRNGState -> ConsumerConfig MainM MailID Mail
    mailsConsumer awsconf master mslave pool rng = ConsumerConfig {
      ccJobsTable = "mails"
    , ccConsumersTable = "mailer_workers"
    , ccJobSelectors = mailSelectors
    , ccJobFetcher = mailFetcher
    , ccJobIndex = mailID
    , ccNotificationChannel = Just mailNotificationChannel
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs = 10
    , ccProcessJob = \mail@Mail{..} -> localData [identifier_ mailID] $ if isNotSendable mail
      then do
        logInfo "Email is not sendable, discarding" $ object [
            "mail" .= show mail
          ]
        return $ Failed Remove
      else AWS.runAmazonMonadT awsconf . runCryptoRNGT rng $ do
        senderType <- if mailServiceTest
          then return MasterSender
          else withPostgreSQL pool $ dbQuery GetCurrentSenderType
        sender <- case senderType of
          MasterSender -> return master
          SlaveSender -> case mslave of
            Just slave -> return slave
            Nothing -> do
              logAttention_ "No slave sender, falling back to master"
              return master
        logInfo "Sending email" $ object [
            "sender" .= show sender
          ]
        sendMail sender mail >>= \case
          True  -> return $ Ok MarkProcessed
          False -> Failed <$> sendoutFailed mail
    , ccOnException = const sendoutFailed
    }
      where
        isNotSendable Mail{..} =
          null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo

        sendoutFailed Mail{..} = do
          logInfo_ "Failed to send email"
          if mailAttempts < 100
            then do
              logInfo_ "Deferring email for 5 minutes"
              return . RerunAfter $ iminutes 5
            else do
              logInfo_ "Deleting email since there were 100 unsuccessful attempts to send it"
              return Remove

    jobsWorker :: MailingServerConf -> ConnectionSource -> CryptoRNGState
               -> ConsumerConfig MainM JobType MailerJob
    jobsWorker conf pool rng = ConsumerConfig {
      ccJobsTable = "mailer_jobs"
    , ccConsumersTable = "mailer_workers"
    , ccJobSelectors = mailerJobSelectors
    , ccJobFetcher = mailerJobFetcher
    , ccJobIndex = mjType
    , ccNotificationChannel = Nothing
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs = 1
    , ccProcessJob = \MailerJob{..} -> runCryptoRNGT rng . logHandlerInfo mjType $ case mjType of
      CleanOldEmails -> do
        let daylimit = 14
        logInfo_ $ "Removing emails sent" <+> T.pack (show daylimit) <+> "days ago."
        cleaned <- withPostgreSQL pool . dbUpdate $ CleanEmailsOlderThanDays daylimit
        logInfo "Old emails removed" $ object [
            "removed" .= cleaned
          ]
        Ok . RerunAt . nextDayMidnight <$> currentTime

      PerformServiceTest -> case mscSlaveSender conf of
        -- If there is no slave sender, retry periodically to be
        -- able to start the process if the configuration changes.
        Nothing -> withPostgreSQL pool $ do
          dbUpdate $ CollectServiceTestResultIn $ iseconds 50
          return $ Ok MarkProcessed
        Just _ -> withPostgreSQL pool . runCryptoRNGT rng $ do
          logInfo_ "Running service checker"
          token <- random
          mid <- dbUpdate $ CreateServiceTest (token, testSender, testReceivers conf, Just testSender, "test", "test", [], mempty)
          logInfo "Service testing email created" $ object [
              identifier_ mid
            ]
          dbUpdate $ CollectServiceTestResultIn $ iminutes 10
          return $ Ok MarkProcessed

      CollectServiceTestResult -> case mscSlaveSender conf of
        Nothing -> withPostgreSQL pool $ do
          dbUpdate ScheduleServiceTest
          return $ Ok MarkProcessed
        Just _ -> withPostgreSQL pool . runCryptoRNGT rng $ do
          events <- dbQuery GetServiceTestEvents
          result <- if any isDelivered events
            then do
              logInfo_ "Service testing emails were delivered successfully"
              return Ok
            else do
              logInfo_ "Service testing emails failed to be delivered within 10 minutes"
              sender <- dbQuery GetCurrentSenderType
              when (sender == MasterSender) $ do
                logInfo_ "Switching to slave sender and resending all emails that were sent within this time"
                dbUpdate SwitchToSlaveSenderImmediately
                resent <- dbUpdate ResendEmailsSentAfterServiceTest
                logInfo "Emails set to be resent" $ object [
                    "emails" .= resent
                  ]
              return Failed
          dbUpdate ScheduleServiceTest
          return $ result MarkProcessed
    , ccOnException = \_ _ -> return . RerunAfter $ ihours 1
    }
      where
        logHandlerInfo jobType = localRandomID "job_id" . localData ["job_type" .= show jobType]

        isDelivered (_, _, _, SendGridEvent _ SG_Delivered{} _) = True
        isDelivered (_, _, _, MailGunEvent _ MG_Delivered) = True
        isDelivered _ = False

        testSender = Address {
          addrName = "Scrive mailer"
        , addrEmail = "noreply@scrive.com"
        }
