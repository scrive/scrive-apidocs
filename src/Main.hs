module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Version
import Happstack.Server
import Happstack.StaticRouting
import System.Directory
import System.Environment
import System.IO
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
import Control.Logic
import qualified Amazon as AWS
import qualified Log
import qualified MemCache
import qualified Paths_kontrakcja as Paths
import qualified System.Mem
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

  -- try to create directory for storing documents locally
  when (not . null $ docstore appConf) $
    createDirectoryIfMissing True $ docstore appConf

  withPostgreSQL (dbConfig appConf) $
    performDBChecks Log.server kontraTables kontraMigrations

  startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> IO ()
startSystem appGlobals appConf = E.bracket
  (openAcidState Log.server $ store appConf)
  (\st -> do
    createCheckpoint Log.server st
    closeAcidState Log.server st
  )
  startHttpServer
  where
    startHttpServer appState =
      E.bracket createThreads (mapM_ killThread) waitForTerm
      where
        createThreads = do
          let (iface,port) = httpBindAddress appConf
          listensocket <- listenOn (htonl iface) (fromIntegral port)
          let (routes,overlaps) = compile staticRoutes
          maybe (return ()) Log.server overlaps
          t1 <- forkIO $ simpleHTTPWithSocket listensocket (nullConf { port = fromIntegral port, timeout = 120}) (appHandler routes appConf appGlobals appState)
          let runScheduler = runQueue (cryptorng appGlobals) (dbConfig appConf) (SchedulerData appConf (templates appGlobals) appState)
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
          t6 <- forkIO $ cron 5 $ runScheduler processEvents
          t7 <- forkIO $ if AWS.isAWSConfigOk appConf
                           then cron 60 $ runScheduler AWS.uploadFilesToAmazon
                           else return ()
          t8 <- forkIO $ cron (60 * 60) (System.Mem.performGC >> Log.debug "Performing GC...")
          -- transition function between non-encrypted and encrypted files.
          t9 <- forkIO $ runScheduler AWS.calculateChecksumAndEncryptOldFiles
          return [t1, t2, t3, t4, t5, t6, t7, t8, t9]
        waitForTerm _ = E.bracket
          -- checkpoint the state once a day
          -- FIXME: make it checkpoint always at the same time
          (forkIO $ cron (60*60*24) (createCheckpoint Log.server appState))
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
      _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) Nothing Nothing (mkLocaleFromRegion defaultValue)
      return ()
    Just _ -> return () -- user exist, do not add it
