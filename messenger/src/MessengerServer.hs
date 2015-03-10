module MessengerServer where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Monad.Base
import Data.Monoid
import Data.Monoid.Utils
import Data.Time
import Happstack.Server hiding (waitForTermination)
import qualified Control.Exception.Lifted as E
import qualified Happstack.StaticRouting as R

import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Handlers
import JobQueue.Components
import JobQueue.Config
import MessengerServerConf
import MinutesTime
import OurPrelude
import Sender
import SMS.Data
import SMS.Model
import SMS.Tables
import Utils.IO
import Utils.Network
import qualified Log

main :: IO ()
main = Log.withLogger $ do
  conf <- readConfig Log.mixlog_ "messenger_server.conf"
  checkExecutables

  let cs = def { csConnInfo = mscDBConfig conf }
  withPostgreSQL (simpleSource cs) $
    checkDatabase Log.mixlog_ [] messengerTables
  pool <- liftBase $ createPoolSource cs
  rng <- newCryptoRNGState

  E.bracket (startServer pool rng conf) (liftBase killThread) . const $ do
    let sender = createSender $ mscMasterSender conf
        smsLogger domain msg = Log.mixlog_ $ "SMS:" <+> domain <> ":" <+> msg
        jobsLogger domain msg = Log.mixlog_ $ "Messenger jobs:" <+> domain <> ":" <+> msg
    withConsumer (jobsWorker pool) pool jobsLogger $ do
      withConsumer (smsConsumer rng sender) pool smsLogger $ do
        liftBase waitForTermination
  where
    startServer pool rng conf = do
      let (iface, port) = mscHttpBindAddress conf
          handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          Log.mixlog_ e
          $unexpectedErrorM "static routing"
        Right r -> return (r >>= maybe (notFound (toResponse ("Not found."::String))) return)
      socket <- liftBase (listenOn (htonl iface) $ fromIntegral port)
      fork . liftBase . simpleHTTPWithSocket socket handlerConf . mapServerPartT Log.withLogger $ router rng pool routes

    smsConsumer rng sender = ConsumerConfig {
      ccJobsTable = "smses"
    , ccConsumersTable = "messenger_workers"
    , ccJobSelectors = smsSelectors
    , ccJobFetcher = smsFetcher
    , ccJobIndex = smID
    , ccNotificationChannel = Just smsNotificationChannel
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs = 10
    , ccProcessJob = \sms@ShortMessage{..} -> runCryptoRNGT rng $ do
      Log.mixlog_ $ "Sending sms" <+> show smID
      sendSMS sender sms >>= \case
        True  -> return $ Ok MarkProcessed
        False -> Failed <$> sendoutFailed sms
    , ccOnException = sendoutFailed
    }
      where
        sendoutFailed ShortMessage{..} = do
          Log.mixlog_ $ "Failed to send sms" <+> show smID
          if smAttempts < 100
            then do
              Log.mixlog_ $ "Deferring sms" <+> show smID <+> "for 5 minutes"
              return . RetryAfter $ iminutes 5
            else do
              Log.mixlog_ $ "Deleting sms" <+> show smID <+> "since there was over 100 tries to send it"
              return Remove

    jobsWorker pool = ConsumerConfig {
      ccJobsTable = "messenger_jobs"
    , ccConsumersTable = "messenger_workers"
    , ccJobSelectors = messengerJobSelectors
    , ccJobFetcher = messengerJobFetcher
    , ccJobIndex = mjType
    , ccNotificationChannel = Nothing
    , ccNotificationTimeout = 10 * 60 * 1000000 -- 10 minutes
    , ccMaxRunningJobs = 1
    , ccProcessJob = \MessengerJob{..} -> case mjType of
      CleanOldSMSes -> do
        let daylimit = 3
        Log.mixlog_ $ "Removing smses sent" <+> show daylimit <+> "days ago."
        cleaned <- withPostgreSQL pool . dbUpdate $ CleanSMSesOlderThanDays daylimit
        Log.mixlog_ $ show cleaned <+> "smses were removed."
        now <- currentTime
        -- run at midnight
        return . Ok $ RetryAt UTCTime {
          utctDay = 1 `addDays` utctDay now
        , utctDayTime = 0
        }
    , ccOnException = const . return . RetryAfter $ ihours 1
    }
