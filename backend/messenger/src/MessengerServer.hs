module MessengerServer (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Happstack.Server hiding (waitForTermination)
import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T
import qualified Happstack.StaticRouting as R

import Configuration
import Crypto.RNG
import DB
import DB.PostgreSQL
import Handlers
import Happstack.Server.ReqHandler
import KontraPrelude
import Log.Configuration
import Log.Identifier
import MessengerServerConf
import MinutesTime
import Sender
import SMS.Data
import SMS.Model
import SMS.Tables
import Utils.IO
import Utils.Network

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
    configFile = "messenger_server.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  conf <- readConfig putStrLn config
  rng <- newCryptoRNGState
  lr@LogRunner{..} <- mkLogRunner "messenger" (mscLogConfig conf) rng
  withLogger $ do
    checkExecutables

    let pgSettings = pgConnSettings (mscDBConfig conf) []
    withPostgreSQL (unConnectionSource $ simpleSource pgSettings) $
      checkDatabase [] messengerTables
    cs@(ConnectionSource pool) <- ($ maxConnectionTracker)
      <$> liftBase (createPoolSource pgSettings)

    let cron = jobsWorker cs
        sender = smsConsumer rng cs $ createSender $ sendersConfigFromMessengerConf conf
    E.bracket (startServer lr cs rng conf) (liftBase killThread) . const
      . finalize (localDomain "cron" $ runConsumer cron pool)
      . finalize (localDomain "sender" $ runConsumer sender pool) $ do
      liftBase waitForTermination
  where
    startServer
      :: LogRunner
      -> TrackedConnectionSource
      -> CryptoRNGState
      -> MessengerServerConf
      -> MainM ThreadId
    startServer LogRunner{..} cs rng conf = do
      let (iface, port) = mscHttpBindAddress conf
          handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          logInfo "Error while compiling routes" $ object [
              "error" .= e
            ]
          $unexpectedErrorM "static routing"
        Right r -> return $ r >>= maybe (notFound $ toResponse ("Not found."::String)) return
      socket <- liftBase (listenOn (htonl iface) $ fromIntegral port)
      fork . liftBase . runReqHandlerT socket handlerConf . withLogger $ router rng cs routes

    smsConsumer
      :: CryptoRNGState
      -> TrackedConnectionSource
      -> Sender
      -> ConsumerConfig MainM ShortMessageID ShortMessage
    smsConsumer rng (ConnectionSource pool) sender = ConsumerConfig {
      ccJobsTable = "smses"
    , ccConsumersTable = "messenger_workers"
    , ccJobSelectors = smsSelectors
    , ccJobFetcher = smsFetcher
    , ccJobIndex = smID
    , ccNotificationChannel = Just smsNotificationChannel
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs = 10
    , ccProcessJob = \sms@ShortMessage{..} -> localData [identifier_ smID] . runCryptoRNGT rng $ do
      logInfo_ "Sending sms"
      withPostgreSQL pool $ sendSMS sender sms >>= \case
        True  -> return $ Ok MarkProcessed
        False -> Failed <$> sendoutFailed sms
    , ccOnException = const sendoutFailed
    }
      where
        sendoutFailed ShortMessage{..} = do
          logInfo_ "Failed to send sms"
          if smAttempts < 100
            then do
              logInfo_ "Deferring sms for 5 minutes"
              return . RerunAfter $ iminutes 5
            else do
              logInfo_ "Deleting sms since there was over 100 tries to send it"
              return Remove

    jobsWorker
      :: TrackedConnectionSource
      -> ConsumerConfig MainM JobType MessengerJob
    jobsWorker (ConnectionSource pool) = ConsumerConfig {
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
        logInfo_ $ "Removing smses sent" <+> T.pack (show daylimit) <+> "days ago"
        cleaned <- withPostgreSQL pool . dbUpdate $ CleanSMSesOlderThanDays daylimit
        logInfo "Old smses removed" $ object [
            "removed" .= cleaned
          ]
        Ok . RerunAt . nextDayMidnight <$> currentTime
    , ccOnException = \_ _ -> return . RerunAfter $ ihours 1
    }
