module KontrakcjaServer (
    runKontrakcjaServer
  , runTest
  ) where

import Control.Concurrent (forkIO, killThread)
import Happstack.Util.Cron (cron)
import Happstack.Server
  ( Conf(port)
  , simpleHTTPWithSocket
  , nullConf
  )
import Happstack.StaticRouting (compile)
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , runTxSystem
  , shutdownSystem
  , createCheckpoint
  , waitForTermination
  )
import System.Environment
import System.Directory (createDirectoryIfMissing)
import qualified AppLogger as Log
import AppState (AppState)
import RoutingTable (staticRoutes)
import AppControl
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import System.IO
import Control.Concurrent.MVar
--import Control.Monad.Reader

import AppDB
import Configuration
import DB.Checks
import DB.Classes
import Database.HDBC.PostgreSQL
import Network
import qualified Control.Exception as E
import Happstack.State.Saver
import ActionScheduler
import ActionSchedulerState (ActionImportance(..), SchedulerData(..))
import User.Model
-- import qualified User.UserState as U
import qualified Amazon as AWS
import Mails.MailsConfig
import Mails.SendMail
import Templates.Templates (readGlobalTemplates, getTemplatesModTime)
import Misc
import qualified MemCache
import File.Model
import qualified System.Mem as System.Mem
import qualified Doc.Import as D

startTestSystemState' :: (Component st, Methods st) => Proxy st -> IO (MVar TxControl)
startTestSystemState' proxy = do
  runTxSystem NullSaver proxy

runTest :: IO () -> IO ()
runTest test = do
  E.bracket
               -- start the state system
              (startTestSystemState' stateProxy)
              (\control -> do
                  shutdownSystem control)
              (\_control -> do
                 test)

stateProxy :: Proxy AppState
stateProxy = Proxy

initDatabaseEntries :: Connection -> [(Email,String)] -> IO ()
initDatabaseEntries conn iusers = do
  -- create initial database entries
  flip mapM_ iusers $ \(email,passwordstring) -> do
      passwd <- createPassword (BS.pack passwordstring)
      maybeuser <- ioRunDB conn $ dbQuery $ GetUserByEmail Nothing email
      case maybeuser of
          Nothing -> do
              _ <- ioRunDB conn $ dbUpdate $ AddUser (BS.empty, BS.empty) (unEmail email) (Just passwd) False Nothing Nothing (mkLocaleFromRegion defaultValue)
              return ()
          Just _ -> return () -- user exist, do not add it

uploadFileToAmazon :: AppConf -> IO Bool
uploadFileToAmazon appConf = do
  withPostgreSQL (dbConfig appConf) $ \conn -> ioRunDB conn $ do
    mfile <- dbQuery $ GetFileThatShouldBeMovedToAmazon
    case mfile of
      Just file -> do
        AWS.uploadFile (docstore appConf) (defaultAWSAction appConf) file
        return True
      _ -> return False

runKontrakcjaServer :: IO ()
runKontrakcjaServer = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appname <- getProgName
  args <- getArgs
  appConf <- readConfig Log.server appname args "kontrakcja.conf"
  templates' <- readGlobalTemplates
  templateModTime <- getTemplatesModTime
  templates <- newMVar (templateModTime, templates')

  filecache' <- MemCache.new (BS.length) 50000000

  let mailer' = case cfg of
                   MailsSendgrid{} -> createSendgridMailer cfg
                   MailsSendmail{} -> createSendmailMailer cfg
                   MailsLocalOpen{} -> createLocalOpenMailer (ourInfoEmail cfg) (ourInfoEmailNiceName cfg)
                 where cfg = mailsConfig appConf

  -- variable for cached documents
  docs <- newMVar Map.empty
  -- variable for enforcing emails sendout
  es_enforcer <- newEmptyMVar

  -- try to create directory for storing documents locally
  if null $ docstore appConf
     then return ()
     else createDirectoryIfMissing True $ docstore appConf

  withPostgreSQL (dbConfig appConf) $ \conn -> do
    res <- ioRunDB conn $ tryDB $ performDBChecks Log.server kontraTables kontraMigrations
    case res of
      Left (e::E.SomeException) -> do
        Log.error $ "Error while checking DB consistency: " ++ show e
      Right _ -> do
        let appGlobals = AppGlobals {
            templates = templates
          , filecache = filecache'
          , mailer = mailer'
          , appbackdooropen = isBackdoorOpen $ mailsConfig appConf
          , docscache = docs
          , esenforcer = es_enforcer
        }
        E.bracket
                 -- start the state system
              (do
                  Log.server $ "Using store " ++ store appConf
                  startSystemState' (store appConf) stateProxy)
              (\control -> do
                  Log.server $ "Creating checkpoint before exit"
                  createCheckpoint control
                  Log.server $ "Closing transaction system"
                  shutdownSystem control)
              (\control -> do

                  -- start the http server
                  E.bracket
                           (do
                              D.populateDBWithDocumentsIfEmpty conn

                              let (iface,port) = httpBindAddress appConf
                              listensocket <- listenOn (htonl iface) (fromIntegral port)
                              let (routes,overlaps) = compile staticRoutes
                              maybe (return ()) Log.server overlaps
                              t1 <- forkIO $ simpleHTTPWithSocket listensocket (nullConf { port = fromIntegral port })
                                    (appHandler routes appConf appGlobals)
                              let scheddata = SchedulerData appConf mailer' templates es_enforcer
                              t2 <- forkIO $ cron 60 $ runScheduler (oldScheduler >> actionScheduler UrgentAction) scheddata
                              t3 <- forkIO $ cron 600 $ runScheduler (actionScheduler LeisureAction) scheddata
                              t4 <- forkIO $ runEnforceableScheduler 300 es_enforcer (actionScheduler EmailSendoutAction) scheddata
                              t5 <- forkIO $ cron (60 * 60 * 4) $ runScheduler runDocumentProblemsCheck scheddata
                              t6 <- forkIO $ cron (60 * 60 * 24) $ runScheduler runArchiveProblemsCheck scheddata
                              t7 <- forkIO $ cron (60) $ (let loop = (do
                                                                        r <- uploadFileToAmazon appConf
                                                                        if r then loop else return ()) in loop)
                              t8 <- forkIO $ cron (60*60) System.Mem.performGC
                              return [t1, t2, t3, t4, t5, t6, t7, t8]
                           )
                           (mapM_ killThread) $ \_ -> E.bracket
                                        -- checkpoint the state once a day
                                        -- FIXME: make it checkpoint always at the same time
                                        (forkIO $ cron (60*60*24) (createCheckpoint control))
                                        (killThread) $ \_ -> do
                                          initDatabaseEntries conn (initialUsers appConf)
                                          -- wait for termination signal
                                          waitForTermination
                                          Log.server $ "Termination request received"

                  return ())

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' = runTxSystem . Queue . FileSaver
