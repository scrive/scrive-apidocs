module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Version
import Happstack.Server
import Happstack.StaticRouting
import Happstack.State
import Happstack.Util.Cron
import System.Directory
import System.Environment
import System.IO
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

import ActionScheduler
import ActionSchedulerState
import AppControl
import AppDB
import AppState
import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import Mails.Events
import Misc
import Network
import RoutingTable
import Templates.TemplatesLoader
import User.Model
import qualified Amazon as AWS
import qualified Log
import qualified MemCache
import qualified Paths_kontrakcja as Paths
import qualified System.Mem

main :: IO ()
main = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  Log.server $ "Starting kontrakcja-server build " ++ concat (intersperse "." (versionTags Paths.version))

  appConf <- getProgName
    >>= \appname -> getArgs
    >>= \args -> readConfig Log.server appname args "kontrakcja.conf"

  appGlobals <- (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
    >>= \templates -> MemCache.new BS.length 50000000
    >>= \filecache -> newMVar Map.empty
    >>= \docs -> newCryptoRNGState
    >>= \rng -> return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      }

  -- try to create directory for storing documents locally
  when (not . null $ docstore appConf) $
    createDirectoryIfMissing True $ docstore appConf

  withPostgreSQL (dbConfig appConf) $
    performDBChecks Log.server kontraTables kontraMigrations

  startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> IO ()
startSystem appGlobals appConf =
  E.bracket startStateSystem createCheckpointAndExit startHttpServer
  where
    startStateSystem = do
      Log.server $ "Using store " ++ store appConf
      runTxSystem (Queue $ FileSaver (store appConf)) (Proxy :: Proxy AppState)

    createCheckpointAndExit control = do
      Log.server $ "Creating checkpoint before exit"
      createCheckpoint control
      Log.server $ "Closing transaction system"
      shutdownSystem control

    startHttpServer control =
      E.bracket createThreads (mapM_ killThread) waitForTerm
      where
        createThreads = do
          let (iface,port) = httpBindAddress appConf
          listensocket <- listenOn (htonl iface) (fromIntegral port)
          let (routes,overlaps) = compile staticRoutes
          maybe (return ()) Log.server overlaps
          t1 <- forkIO $ simpleHTTPWithSocket listensocket (nullConf { port = fromIntegral port })  (appHandler routes appConf appGlobals)
          let scheddata = SchedulerData appConf $ templates appGlobals
              rng = cryptorng appGlobals
          t2 <- forkIO $ cron 60 $ runScheduler rng (oldScheduler >> actionScheduler UrgentAction) scheddata
          t3 <- forkIO $ cron 600 $ runScheduler rng (actionScheduler LeisureAction) scheddata
          --t4 <- forkIO $ cron (60 * 60 * 4) $ runScheduler rng runDocumentProblemsCheck scheddata
          --t5 <- forkIO $ cron (60 * 60 * 24) $ runScheduler rng runArchiveProblemsCheck scheddata
          t6 <- forkIO $ cron 5 $ runScheduler rng processEvents scheddata
          t7 <- forkIO $ cron 60 $ runScheduler rng AWS.uploadFilesToAmazon scheddata
          t8 <- forkIO $ cron (60 * 60) System.Mem.performGC
          --return [t1, t2, t3, t4, t5, t6, t7, t8]
          return [t1, t2, t3, t6, t7, t8]

        waitForTerm _ = E.bracket
          -- checkpoint the state once a day
          -- FIXME: make it checkpoint always at the same time
          (forkIO $ cron (60*60*24) (createCheckpoint control))
          killThread $ \_ -> do
            withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $
              initDatabaseEntries $ initialUsers appConf
            -- wait for termination signal
            waitForTermination
            Log.server $ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m) => [(Email, String)] -> m ()
initDatabaseEntries = mapM_ $ \(email, passwordstring) -> do
  -- create initial database entries
  passwd <- createPassword passwordstring
  maybeuser <- dbQuery $ GetUserByEmail Nothing email
  case maybeuser of
    Nothing -> do
      _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
      return ()
    Just _ -> return () -- user exist, do not add it
