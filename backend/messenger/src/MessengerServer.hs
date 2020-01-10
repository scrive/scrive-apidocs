module MessengerServer (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Crypto.RNG
import Database.PostgreSQL.Consumers
import Database.PostgreSQL.PQTypes.Checks
import Happstack.Server hiding (waitForTermination)
import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.FilePath ((</>), FilePath)
import qualified Control.Exception.Lifted as E
import qualified Data.Text.IO as T
import qualified Happstack.StaticRouting as R

import AppDir (AppPaths(..), setupAppPaths)
import Configuration
import DB
import DB.PostgreSQL
import Handlers
import Happstack.Server.ReqHandler
import Log.Configuration
import Log.Identifier
import MessengerServerConf
import MinutesTime
import Monitoring
import Sender
import SMS.Model
import SMS.Tables
import SMS.Types
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
  where configFile = workspaceRoot </> "messenger_server.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = do
  (AppPaths _ workspaceRoot) <- setupAppPaths
  CmdConf {..}               <- cmdArgs . cmdConf workspaceRoot =<< getProgName
  conf                       <- readConfig putStrLn config
  case messengerMonitoringConfig conf of --
    Just mconf -> void $ startMonitoringServer mconf
    Nothing    -> return ()
  rng        <- newCryptoRNGState
  (errs, lr) <- mkLogRunner "messenger" (messengerLogConfig conf) rng
  mapM_ T.putStrLn errs

  runWithLogRunner lr $ do
    checkExecutables

    let pgSettings    = pgConnSettings (messengerDBConfig conf) []
        extrasOptions = defaultExtrasOptions
    withPostgreSQL (unConnectionSource $ simpleSource pgSettings)
      $ checkDatabaseAllowUnknownObjects extrasOptions [] [] messengerTables
    cs@(ConnectionSource pool) <-
      ($ (maxConnectionTracker $ messengerMaxDBConnections conf))
        <$> liftBase (createPoolSource pgSettings (messengerMaxDBConnections conf))

    let cron   = jobsWorker cs
        sender = smsConsumer rng cs $ createSender $ sendersConfigFromMessengerConf conf
    E.bracket (startServer cs rng conf) (liftBase killThread)
      . const
      . finalize (localDomain "cron" $ runConsumer cron pool)
      . finalize (localDomain "sender" $ runConsumer sender pool)
      $ do
          liftBase waitForTermination
  where
    startServer
      :: TrackedConnectionSource
      -> CryptoRNGState
      -> MessengerServerConf
      -> MainM ThreadId
    startServer cs rng conf = do
      let (iface, port) = messengerHttpBindAddress conf
          handlerConf   = nullConf { port = fromIntegral port, logAccess = Nothing }
      routes <- case R.compile handlers of
        Left e -> do
          logInfo "Error while compiling routes" $ object ["error" .= e]
          unexpectedError "static routing"
        Right r ->
          return $ r >>= maybe (notFound $ toResponse ("Not found." :: String)) return
      socket <- liftBase (listenOn (htonl iface) $ fromIntegral port)
      fork . mapLogT (runReqHandlerT socket handlerConf) $ router rng cs routes

    smsConsumer
      :: CryptoRNGState
      -> TrackedConnectionSource
      -> Sender
      -> ConsumerConfig MainM ShortMessageID ShortMessage
    smsConsumer rng (ConnectionSource pool) sender = ConsumerConfig
      { ccJobsTable           = "smses"
      , ccConsumersTable      = "messenger_workers"
      , ccJobSelectors        = smsSelectors
      , ccJobFetcher          = smsFetcher
      , ccJobIndex            = smID
      , ccNotificationChannel = Just smsNotificationChannel
      , ccNotificationTimeout = 60 * 1000000 -- 1 minute
      , ccMaxRunningJobs      = 10
      , ccProcessJob          = \sms@ShortMessage {..} ->
                                  localData [identifier smID] . runCryptoRNGT rng $ do
                                    logInfo_ "Sending sms"
                                    withPostgreSQL pool $ sendSMS sender sms >>= \case
                                      True  -> return $ Ok MarkProcessed
                                      False -> Failed <$> sendoutFailed sms
      , ccOnException         = const sendoutFailed
      }
      where
        sendoutFailed ShortMessage {..} = do
          logInfo_ "Failed to send sms"
          if smAttempts < 100
            then do
              logInfo_ "Deferring sms for 5 minutes"
              return . RerunAfter $ iminutes 5
            else do
              logInfo_ "Deleting sms since there was over 100 tries to send it"
              return Remove

    jobsWorker :: TrackedConnectionSource -> ConsumerConfig MainM JobType MessengerJob
    jobsWorker (ConnectionSource pool) = ConsumerConfig
      { ccJobsTable           = "messenger_jobs"
      , ccConsumersTable      = "messenger_workers"
      , ccJobSelectors        = messengerJobSelectors
      , ccJobFetcher          = messengerJobFetcher
      , ccJobIndex            = mjType
      , ccNotificationChannel = Nothing
      , ccNotificationTimeout = 10 * 60 * 1000000 -- 10 minutes
      , ccMaxRunningJobs      = 1
      , ccProcessJob          =
        \MessengerJob {..} -> case mjType of
          CleanOldSMSes -> do
            let daylimit = 3
            logInfo_ $ "Removing smses sent" <+> showt daylimit <+> "days ago"
            cleaned <- withPostgreSQL pool . dbUpdate $ CleanSMSesOlderThanDays daylimit
            logInfo "Old smses removed" $ object ["removed" .= cleaned]
            Ok . RerunAt . nextDayMidnight <$> currentTime
      , ccOnException         = \_ _ -> return . RerunAfter $ ihours 1
      }
