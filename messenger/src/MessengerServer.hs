module MessengerServer where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Data.Time
import Happstack.Server hiding (waitForTermination)
import Log
import qualified Control.Exception.Lifted as E
import qualified Happstack.StaticRouting as R

import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Handlers
import Happstack.Server.ReqHandler
import JobQueue.Components
import JobQueue.Config
import JobQueue.Utils
import KontraPrelude
import Log.Configuration
import MessengerServerConf
import Sender
import SMS.Data
import SMS.Model
import SMS.Tables
import Utils.IO
import Utils.Network

type MainM = LogT IO

main :: IO ()
main = do
  conf <- readConfig putStrLn "messenger_server.conf"
  lr@LogRunner{..} <- mkLogRunner "messenger" $ mscLogConfig conf
  withLogger $ do
    checkExecutables

    let cs = def { csConnInfo = mscDBConfig conf }
    withPostgreSQL (simpleSource cs) $
      checkDatabase logInfo_ [] messengerTables
    pool <- liftBase $ createPoolSource (liftBase . withLogger . logAttention_) cs
    rng <- newCryptoRNGState

    E.bracket (startServer lr pool rng conf) (liftBase killThread) . const $ do
      let cron = jobsWorker pool
          sender = smsConsumer rng $ createSender $ mscMasterSender conf
      finalize (localDomain "cron" $ runConsumer cron pool) $ do
        finalize (localDomain "sender" $ runConsumer sender pool) $ do
          liftBase waitForTermination
  where
    startServer :: LogRunner -> ConnectionSource
                -> CryptoRNGState -> MessengerServerConf -> MainM ThreadId
    startServer LogRunner{..} pool rng conf = do
      let (iface, port) = mscHttpBindAddress conf
          handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          logInfo_ e
          $unexpectedErrorM "static routing"
        Right r -> return $ r >>= maybe (notFound $ toResponse ("Not found."::String)) return
      socket <- liftBase (listenOn (htonl iface) $ fromIntegral port)
      fork . liftBase . runReqHandlerT socket handlerConf . mapReqHandlerT withLogger $ router rng pool routes

    smsConsumer :: CryptoRNGState -> Sender
                -> ConsumerConfig MainM ShortMessageID ShortMessage
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
      logInfo_ $ "Sending sms" <+> show smID
      sendSMS sender sms >>= \case
        True  -> return $ Ok MarkProcessed
        False -> Failed <$> sendoutFailed sms
    , ccOnException = const sendoutFailed
    }
      where
        sendoutFailed ShortMessage{..} = do
          logInfo_ $ "Failed to send sms" <+> show smID
          if smAttempts < 100
            then do
              logInfo_ $ "Deferring sms" <+> show smID <+> "for 5 minutes"
              return . RerunAfter $ iminutes 5
            else do
              logInfo_ $ "Deleting sms" <+> show smID <+> "since there was over 100 tries to send it"
              return Remove

    jobsWorker :: ConnectionSource
               -> ConsumerConfig MainM JobType MessengerJob
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
        logInfo_ $ "Removing smses sent" <+> show daylimit <+> "days ago."
        cleaned <- withPostgreSQL pool . dbUpdate $ CleanSMSesOlderThanDays daylimit
        logInfo_ $ show cleaned <+> "smses were removed."
        now <- currentTime
        -- run at midnight
        return . Ok $ RerunAt UTCTime {
          utctDay = 1 `addDays` utctDay now
        , utctDayTime = 0
        }
    , ccOnException = \_ _ -> return . RerunAfter $ ihours 1
    }
