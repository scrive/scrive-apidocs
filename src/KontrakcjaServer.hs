module KontrakcjaServer (defaultConf,
                         runKontrakcjaServer,
                         runTest,
                         readAppConfig ) where

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
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import System.Directory (createDirectoryIfMissing)
import qualified AppLogger as Log
import AppState (AppState)
import RoutingTable (staticRoutes)
import AppControl (appHandler,defaultAWSAction,AppConf(..),AppGlobals(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map
import System.IO
import Control.Concurrent.MVar
import Control.Monad.Reader

import Crypto
import DB.Classes
import DB.Migrations
import Database.HDBC.PostgreSQL
import Network.BSD
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo )
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

{- | Getting application configuration. Reads 'kontrakcja.conf' from current directory
     Setting production param can change default setting (not to send mails)
-}
readAppConfig :: IO AppConf
readAppConfig = readConf `catch` printDefault
    where
      filepath = "kontrakcja.conf"
      readConf = do
        h <- openFile filepath ReadMode
        hSetEncoding h utf8
        c <- hGetContents h
        let c' = unwords (lines c)
        conf <- readIO c'
        hClose h
        Log.server $ "App config file " ++ filepath ++" read and parsed"
        case verifyAESConf $ aesConfig conf of
             Left err -> error err
             _        -> return ()
        return conf
      printDefault ex = do
        Log.server $ "No app config provided. Exiting now. Error: " ++ show ex
        Log.server $ "Please provide application config file " ++ filepath
        Log.server $ "Example configuration:"
        Log.server $ show (defaultConf "kontrakcja")
        error "Config file error"


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

{-
   Network.listenOn bind randomly to IPv4 or IPv6 or both,
   depending on system and local settings.
   Lets make it use IPv4 only
-}

listenOn :: HostAddress -> PortNumber -> IO Socket
listenOn iface port = do
    proto <- getProtocolNumber "tcp"
    E.bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet port iface)
            listen sock maxListenQueue
            return sock
        )

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
  withPostgreSQL (dbConfig appConf) $ \conn -> do
    mfile <- ioRunDB conn $ dbQuery $ GetFileThatShouldBeMovedToAmazon
    case mfile of
        Just file -> do
                   runReaderT (AWS.uploadFile (docstore appConf) (defaultAWSAction appConf) file) conn
                   return True
        _ -> return False

runKontrakcjaServer :: IO ()
runKontrakcjaServer = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  appConf1 <- readAppConfig
  templates' <- readGlobalTemplates
  templateModTime <- getTemplatesModTime
  templates <- newMVar (templateModTime, templates')

  filecache' <- MemCache.new (BS.length) 50000000

  let mailer' = case cfg of
                   MailsSendgrid{} -> createSendgridMailer cfg
                   MailsSendmail{} -> createSendmailMailer cfg
                   MailsLocalOpen{} -> createLocalOpenMailer (ourInfoEmail cfg) (ourInfoEmailNiceName cfg)
                 where cfg = mailsConfig appConf1

  -- variable for cached documents
  docs <- newMVar Map.empty
  -- variable for enforcing emails sendout
  es_enforcer <- newEmptyMVar

  appConf <- case parseConfig args of
    (Left e) -> do Log.server (unlines e)
                   exitFailure
    (Right f) -> return $ (f (appConf1))

  -- try to create directory for storing documents locally
  if null $ docstore appConf
     then return ()
     else createDirectoryIfMissing True $ docstore appConf

  withPostgreSQL (dbConfig appConf) $ \conn -> do
    res <- ioRunDB conn $ tryDB checkDBConsistency
    case res of
      Left (e::E.SomeException) -> do
        Log.error $ "Error while checking DB consistency: " ++ show e
      Right _ -> do
        let appGlobals = AppGlobals {
            templates = templates
          , filecache = filecache'
          , mailer = mailer'
          , appbackdooropen = isBackdoorOpen $ mailsConfig appConf1
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
                              -- populate db with entries from happstack-state
                              ioRunDB conn $ do
                                  -- U.populateDBWithUsersIfEmpty

                                  -- this is not ready yet
                                  --populateDBWithDocumentsIfEmpty

                                  return ()

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

{- | Default application configuration that does nothing.

sign url    "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
admin url   "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
storage url "https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc"
-}
defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpBindAddress    = (0x7f000001, 8000)
              , hostpart           = "http://localhost:8000"
              , store              = "_local/" ++ progName ++ "_state"
              , docstore           = "_local/documents"
              , static             = "public"
              , amazonConfig       = Nothing
              , dbConfig           = "user='user' password='password' dbname='kontrakcja'"
              , gsCmd              = "gs"
              , production         = False
              , trustWeaverSign    = Nothing
              , trustWeaverAdmin   = Nothing
              , trustWeaverStorage = Nothing
              , mailsConfig        = defaultMailConfig
              , aesConfig          = AESConf {
                    aesKey = BS.pack "}>\230\206>_\222\STX\218\SI\159i\DC1H\DC3Q\ENQK\r\169\183\133bu\211\NUL\251s|\207\245J"
                  , aesIV = BS.pack "\205\168\250\172\CAN\177\213\EOT\254\190\157SY3i\160"
                  }
              , admins             = map (Email . BSU.fromString) ["gracjanpolak@gmail.com", "lukas@skrivapa.se"]
              , initialUsers       = []
              }

opts :: [OptDescr (AppConf -> AppConf)]
opts = [
       {-
       , Option [] ["no-validate"]
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Nothing } }))
         "Turn off HTML validation"
       , Option [] ["validate"]
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } }))
         "Turn on HTML validation"
       -}
          Option [] ["store"]
         (ReqArg (\h c -> c {store = h}) "PATH")
         "The directory used for database storage."
       , Option [] ["static"]
         (ReqArg (\h c -> c {static = h}) "PATH")
         "The directory searched for static files"
       , Option [] ["production"]
         (NoArg (\ c -> c { production = True }))
         "Turn on production environment"
       ]

parseConfig :: [String] -> Either [String] (AppConf -> AppConf)
parseConfig args
    = case getOpt Permute opts args of
        (flags,_,[]) -> Right $ \appConf -> foldr ($) appConf flags
        (_,_,errs)   -> Left errs

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' = runTxSystem . Queue . FileSaver

