module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Version
import Happstack.Server (simpleHTTPWithSocket, nullConf, port, timeout)
import Happstack.StaticRouting
import System.Environment
import System.IO
import qualified System.Time
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import ActionQueue.EmailChangeRequest
import ActionQueue.Monad
import ActionQueue.PasswordReminder
import ActionQueue.Scheduler
import ActionQueue.UserAccountRequest
import AppConf
import AppControl
import AppDB
import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.SQLFunction
import DB.PostgreSQL
import Doc.API.Callback.Model
import Utils.Cron
import Utils.Default
import Mails.Events
import Utils.IO
import MinutesTime
import Utils.Network
import Payments.Config
import Payments.Control
import RoutingTable
import Session.Data
import Templates.TemplatesLoader
import User.Model
import Control.Logic
import qualified Amazon as AWS
import qualified Log
import qualified MemCache
import qualified Paths_kontrakcja as Paths
import qualified Static.Resources as SR
import qualified Doc.JpegPages as JpegPages

main :: IO ()
main = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  Log.server $ "Starting kontrakcja-server build " ++ concat (intersperse "." (versionTags Paths.version))

  appConf <- getProgName
    >>= \appname -> getArgs
    >>= \args -> readConfig Log.server appname args "kontrakcja.conf"

  -- Generating static resources (JS and CSS). For development this does nothing. For production it generates joins.
  staticResources' <- SR.getResourceSetsForImport (SR.Production <| production appConf |> SR.Development) (srConfig appConf) ""
  staticResources <- case staticResources' of
                          Right r -> newMVar r
                          Left s -> error $ "Error while generating static resources: " ++ s
  appGlobals <- (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
    >>= \templates -> MemCache.new BS.length 50000000
    >>= \filecache ->  MemCache.new JpegPages.pagesCount 1000
    >>= \docs -> newCryptoRNGState
    >>= \rng -> return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      , staticResources = staticResources
      }

  withPostgreSQL (dbConfig appConf) $ do
    performDBChecks Log.server kontraTables kontraMigrations
    runDBEnv $ defineMany kontraFunctions

  startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> IO ()
startSystem appGlobals appConf = startHttpServer
  where
    startHttpServer =
      E.bracket createThreads (mapM_ killThread) waitForTerm
      where
        createThreads = do
          let (iface,port) = httpBindAddress appConf
          listensocket <- listenOn (htonl iface) (fromIntegral port)
          let (routes,overlaps) = compile staticRoutes
          maybe (return ()) Log.server overlaps
          t1 <- forkIO $ simpleHTTPWithSocket listensocket (nullConf { port = fromIntegral port, timeout = 120}) (appHandler routes appConf appGlobals)
          let runScheduler = runQueue (cryptorng appGlobals) (dbConfig appConf) (SchedulerData appConf (templates appGlobals))
          t2 <- forkIO $ cron 60 $ do
            Log.debug "Running oldScheduler..."
            runScheduler oldScheduler
          t3 <- forkIO $ cron (60 * 60) $ do
            Log.debug "Evaluating EmailChangeRequest actions..."
            runScheduler $ actionQueue emailChangeRequest
          t4 <- forkIO $ cron (60 * 60) $ do
            Log.debug "Evaluating PasswordReminder actions..."
            runScheduler $ actionQueue passwordReminder
          t5 <- forkIO $ cron (60 * 60) $ do
            Log.debug "Evaluating UserAccountRequest actions..."
            runScheduler $ actionQueue userAccountRequest
          t6 <- forkIO $ cron (60 * 60) $ do
            Log.debug "Evaluating sessions..."
            runScheduler $ actionQueue session
          t7 <- forkIO $ cron 5 $ runScheduler processEvents
          t8 <- forkIO $ cron 10 $ runScheduler $ actionQueue documentAPICallback
          t9 <- forkIO $ if AWS.isAWSConfigOk appConf
                           then cron 60 $ runScheduler AWS.uploadFilesToAmazon
                           else return ()
          t10 <- forkIO $ cron (60 * 60) $
            withPostgreSQL (dbConfig appConf) $
              runCryptoRNGT (cryptorng appGlobals) $ do
                mtime <- getMinutesTime
                ctime <- liftIO $ System.Time.toCalendarTime (toClockTime mtime)
                temps <- liftIO $ maybeReadTemplates (production appConf) (templates appGlobals)
                when (System.Time.ctHour ctime == 0) $ -- midnight
                  handleSyncWithRecurly (hostpart appConf) (mailsConfig appConf) temps (recurlyAPIKey $ recurlyConfig appConf) mtime
          return [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]
        waitForTerm _ = do
          withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $
            initDatabaseEntries $ initialUsers appConf
          -- wait for termination signal
          waitForTermination
          Log.server $ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m) => [(Email, String)] -> m ()
initDatabaseEntries = mapM_ $ \(email, passwordstring) -> do
  -- create initial database entries
  passwd <- createPassword passwordstring
  maybeuser <- dbQuery $ GetUserByEmail email
  case maybeuser of
    Nothing -> do
      _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) Nothing (mkLocaleFromRegion defaultValue)
      return ()
    Just _ -> return () -- user exist, do not add it
