module MailingServer where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Data.Monoid
import Data.Monoid.Utils
import Data.Time
import Happstack.Server hiding (result, waitForTermination)
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Happstack.StaticRouting as R

import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Handlers
import JobQueue.Components
import JobQueue.Config
import MailingServerConf
import Mails.Model
import Mails.Tables
import MinutesTime
import OurPrelude
import Sender
import Utils.IO
import Utils.Network
import qualified Amazon as AWS
import qualified Log
import qualified MemCache

type MainM = Log.LogT IO

main :: IO ()
main = Log.withLogger $ do
  -- All running instances need to have the same configuration.
  conf <- readConfig Log.mixlog_ "mailing_server.conf"
  checkExecutables

  let cs = pgConnSettings $ mscDBConfig conf
  withPostgreSQL (simpleSource $ cs []) $ do
    checkDatabase Log.mixlog_ [] mailerTables
  awsconf <- AWS.AmazonConfig (mscAmazonConfig conf) <$> MemCache.new BS.length 52428800
  pool <- liftBase . createPoolSource $ cs mailerComposites
  rng <- newCryptoRNGState

  E.bracket (startServer conf pool rng) (liftBase . killThread) . const $ do
    let mailsLogger domain msg = Log.mixlog_ $ "Mails:" <+> domain <> ":" <+> msg
        jobsLogger domain msg = Log.mixlog_ $ "Mailer jobs:" <+> domain <> ":" <+> msg
        master = createSender pool $ mscMasterSender conf
        mslave = createSender pool <$> mscSlaveSender conf

    withConsumer (jobsWorker conf pool rng) pool jobsLogger $ do
      withConsumer (mailsConsumer awsconf master mslave pool rng) pool mailsLogger $ do
        liftBase waitForTermination
  where
    startServer :: MailingServerConf -> ConnectionSource -> CryptoRNGState
                -> MainM ThreadId
    startServer conf pool rng = do
      let (iface, port) = mscHttpBindAddress conf
          handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          Log.mixlog_ e
          $unexpectedErrorM "static routing"
        Right r -> return $ r >>= maybe (notFound $ toResponse ("Not found."::String)) return
      socket <- liftBase . listenOn (htonl iface) $ fromIntegral port
      fork . liftBase . simpleHTTPWithSocket socket handlerConf . mapServerPartT Log.withLogger $ router rng pool routes

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
    , ccProcessJob = \mail@Mail{..} -> if isNotSendable mail
      then do
        Log.mixlog_ $ "Email" <+> show mail <+> "is not sendable, discarding."
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
              Log.attention_ "No slave sender, falling back to master"
              return master
        Log.mixlog_ $ "Sending email" <+> show mailID <+> "using" <+> show sender
        sendMail sender mail >>= \case
          True  -> return $ Ok MarkProcessed
          False -> Failed <$> sendoutFailed mail
    , ccOnException = sendoutFailed
    }
      where
        isNotSendable Mail{..} =
          null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo

        sendoutFailed Mail{..} = do
          Log.mixlog_ $ "Failed to send email" <+> show mailID
          if mailAttempts < 100
            then do
              Log.mixlog_ $ "Deferring email" <+> show mailID <+> "for 5 minutes"
              return . RetryAfter $ iminutes 5
            else do
              Log.mixlog_ $ "Deleting email" <+> show mailID <+> "since there were 100 unsuccessful attempts to send it"
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
    , ccProcessJob = \MailerJob{..} -> case mjType of
      CleanOldEmails -> do
        let daylimit = 14
        Log.mixlog_ $ "Removing emails sent" <+> show daylimit <+> "days ago."
        cleaned <- withPostgreSQL pool . dbUpdate $ CleanEmailsOlderThanDays daylimit
        Log.mixlog_ $ show cleaned <+> "emails were removed."
        now <- currentTime
        -- run at midnight
        return . Ok $ RetryAt UTCTime {
          utctDay = 1 `addDays` utctDay now
        , utctDayTime = 0
        }

      PerformServiceTest -> case mscSlaveSender conf of
        -- If there is no slave sender, retry periodically to be
        -- able to start the process if the configuration changes.
        Nothing -> withPostgreSQL pool $ do
          dbUpdate $ CollectServiceTestResultIn $ iseconds 50
          return $ Ok MarkProcessed
        Just _ -> withPostgreSQL pool . runCryptoRNGT rng $ do
          Log.mixlog_ $ "Running service checker"
          token <- random
          mid <- dbUpdate $ CreateServiceTest (token, testSender, testReceivers conf, Just testSender, "test", "test", [], mempty)
          Log.mixlog_ $ "Service testing email" <+> show mid <+> "created."
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
              Log.mixlog_ $ "Service testing emails were delivered successfully."
              return Ok
            else do
              Log.mixlog_ $ "Service testing emails failed to be delivered within 10 minutes."
              sender <- dbQuery GetCurrentSenderType
              when (sender == MasterSender) $ do
                Log.mixlog_ $ "Switching to slave sender and resending all emails that were sent within this time."
                dbUpdate SwitchToSlaveSenderImmediately
                resent <- dbUpdate ResendEmailsSentAfterServiceTest
                Log.mixlog_ $ show resent <+> "emails set to be resent."
              return Failed
          dbUpdate ScheduleServiceTest
          return $ result MarkProcessed
    , ccOnException = const . return . RetryAfter $ ihours 1
    }
      where
        isDelivered (_, _, _, SendGridEvent _ SG_Delivered{} _) = True
        isDelivered (_, _, _, MailGunEvent _ MG_Delivered) = True
        isDelivered _ = False

        testSender = Address {
          addrName = "Scrive mailer"
        , addrEmail = "noreply@scrive.com"
        }
