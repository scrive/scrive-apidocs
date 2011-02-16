module Main where

import Control.Concurrent (MVar, forkIO, killThread)
import Happstack.Util.Cron (cron)
import Happstack.State (waitForTermination)
import Happstack.Server
  ( Conf(port)
  , simpleHTTPWithSocket
  , nullConf
  , validator
  , wdgHTMLValidator
  )
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , runTxSystem
  , shutdownSystem
  , createCheckpoint
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import qualified AppLogger as Log
import AppState (AppState(..))
import AppControl (appHandler,defaultAWSAction,AppConf(..),AppGlobals(..))
import qualified Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BS
import System.IO

import Network.BSD
import Network (PortID(..))
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo )
import qualified Network.Socket as Socket ( accept )
import qualified Control.Exception as Exception
import Happstack.State.Saver
import Scheduler
import Happstack.State (update,query)
import Doc.DocControl
import Doc.DocState
import qualified Amazon as AWS
import Mails.MailsConfig
import Mails.SendMail
import Templates.Templates (readTemplates)
import Kontra
import qualified TrustWeaver as TW
import qualified MemCache

{- | Getting application configuration. Reads 'kontrakcja.conf' from current directory
     Setting production param can change default setting (not to send mails)
-}
readAppConfig :: IO AppConf
readAppConfig = read `catch` printDefault
    where
      filepath = "kontrakcja.conf"
      read = do
        h <- openFile filepath ReadMode
        hSetEncoding h utf8
        c <- hGetContents h
        conf <- readIO c
        hClose h
        Log.server $ "App config file " ++ filepath ++" read and parsed"
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
  Exception.bracket
               -- start the state system
              (startTestSystemState' stateProxy)
              (\control -> do
                  shutdownSystem control)
              (\control -> do
                 test)


stateProxy :: Proxy AppState
stateProxy = Proxy

{-
   Network.listenOn bind randomly to IPv4 or IPv6 or both,
   depending on system and local settings.
   Lets make it use IPv4 only
-}

listenOn :: Int -> IO Socket
listenOn port = do
    proto <- getProtocolNumber "tcp"
    Exception.bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral port) 0x0100007F)
            listen sock maxListenQueue
            return sock
        )

initDatabaseEntries :: IO ()
initDatabaseEntries = do
  -- create initial database entries
  passwdhash <- Kontra.createPassword (BS.pack "admin")
  flip mapM_ Kontra.initialUsers $ \email -> do
      maybeuser <- query $ Kontra.GetUserByEmail email
      case maybeuser of
          Nothing -> do
              update $ Kontra.AddUser BS.empty (Kontra.unEmail email) passwdhash Nothing
              return ()
          Just _ -> return () -- user exist, do not add it
  

uploadOldFilesToAmazon :: AppConf -> IO ()
uploadOldFilesToAmazon appConf = do
  files <- query $ GetFilesThatShouldBeMovedToAmazon
  mapM_ (AWS.uploadFile (defaultAWSAction appConf)) files

main = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let progName = "kontrakcja"

  args <- getArgs
  appConf1 <- readAppConfig
  templates <- readTemplates
  filecache' <- MemCache.new (BS.length) 50000000
  let appGlobals = AppGlobals { templates = templates
                              , filecache = filecache'
                              }

  appConf <- case parseConfig args of
    (Left e) -> do Log.server (unlines e)
                   exitFailure
    (Right f) -> return $ (f (appConf1))

  let 
    mailer | sendMails cfg = createRealMailer cfg
           | otherwise = createDevMailer (ourInfoEmail cfg) (ourInfoEmailNiceName cfg)
    cfg = mailsConfig appConf
    ctx = Context
            { ctxmaybeuser = error "Do not use ctxmaybeuser in actions"
            , ctxhostpart = error "Do not use ctxhostpart in actions"
            , ctxflashmessages = error "Do not use ctxflashmessages in actions"
            , ctxtime = error "The ctxtime should be set per action"
            , ctxnormalizeddocuments = error "Do not use normalized documents in actions"
            , ctxipnumber = error "Do not use peerip in actions"
            , ctxs3action = defaultAWSAction appConf
            , ctxproduction = production appConf
            , ctxtemplates = templates
            , ctxmailer = mailer
            , ctxtwconf = TW.TrustWeaverConf 
                          { TW.signcert = twSignCert appConf
                          , TW.signcertpwd = twSignCertPwd appConf
                          , TW.admincert = twAdminCert appConf
                          , TW.admincertpwd = twAdminCertPwd appConf
                          , TW.signurl = twSignUrl appConf
                          , TW.adminurl = twAdminUrl appConf
                          , TW.storageurl = twStorageUrl appConf
                          , TW.retries = 4
                          , TW.timeout = 30000 --30sek
                          }
            , ctxelegtransactions = error "Do not use ctxelegtransactions in actions"
            , ctxfilecache = filecache'
            }
    

  Exception.bracket
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
                  Exception.bracket 
                           (do 
                              -- we need to use our own listenOn as we want to:
                              -- use only IPv4 addresses
                              -- bind only to 127.0.0.1
                              socket <- listenOn (httpPort appConf)
                              t1 <- forkIO $ simpleHTTPWithSocket socket (nullConf { port = httpPort appConf }) 
                                    (appHandler appConf appGlobals)
                              t2 <- forkIO $ cron 60 $ runScheduler appConf
                              return [t1,t2]
                           )
                           (mapM_ killThread) $ \_ -> Exception.bracket
                                        -- checkpoint the state once a day
                                        -- FIXME: make it checkpoint always at the same time
                                        (forkIO $ cron (60*60*24) (createCheckpoint control))
                                        (killThread) $ \_ -> do
                                          initDatabaseEntries
                                          forkIO $ uploadOldFilesToAmazon appConf
                                          -- wait for termination signal
                                          waitForTermination
                                          Log.server $ "Termination request received" 

                  return ())


defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpPort = 8000
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
              , awsBucket = ""
              , awsSecretKey = ""
              , awsAccessKey = ""
              , production = False
              , twSignCert = ""
              , twSignCertPwd = ""
              , twAdminCert = ""
              , twAdminCertPwd = ""
              , twSignUrl = ""
              , mailsConfig = defaultMailConfig
              , twAdminUrl = ""
              , twStorageUrl = ""
              }

opts :: [OptDescr (AppConf -> AppConf)]
opts = [ Option [] ["http-port"]  
         (ReqArg (\h c -> c { httpPort = read h}) "port") 
         "port to bind http server"
       {-
       , Option [] ["no-validate"] 
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Nothing } })) 
         "Turn off HTML validation"
       , Option [] ["validate"]    
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } })) 
         "Turn on HTML validation"
       -}
       , Option [] ["store"]       
         (ReqArg (\h c -> c {store = h}) "PATH") 
         "The directory used for database storage."
       , Option [] ["static"]      
         (ReqArg (\h c -> c {static = h}) "PATH") 
         "The directory searched for static files" 
       , Option [] ["awsbucket"]      
         (ReqArg (\h c -> c {awsBucket = h}) "NAME") 
         "The bucket name to use for AWS S3 storage" 
       , Option [] ["awssecretkey"]      
         (ReqArg (\h c -> c {awsSecretKey = h}) "BASE64") 
         "The AWS secret key"
       , Option [] ["awsaccesskey"]      
         (ReqArg (\h c -> c {awsAccessKey = h}) "BASE64") 
         "The AWS access key" 
       , Option [] ["twsigncert"]      
         (ReqArg (\h c -> c {twSignCert = h}) "PATH") 
         "The TrustWeaver *.pem file with public and private key (for signing)" 
       , Option [] ["twsigncertpwd"]      
         (ReqArg (\h c -> c {twSignCertPwd = h}) "STRING") 
         "The TrustWeaver password for private key (for signing)" 
       , Option [] ["twadmincert"]      
         (ReqArg (\h c -> c {twAdminCert = h}) "PATH") 
         "The TrustWeaver *.pem file with public and private key (for storage)" 
       , Option [] ["twadmincertpwd"]      
         (ReqArg (\h c -> c {twAdminCertPwd = h}) "STRING") 
         "The TrustWeaver password for private key (for storage)" 
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

