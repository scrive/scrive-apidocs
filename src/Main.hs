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
import System.Log.Logger (Priority(..), logM)
import System.Exit (exitFailure)
import System.Console.GetOpt
import AppLogger (withLogger)
import AppState (AppState(..))
import AppControl (appHandler,defaultAWSAction,AppConf(..))
import qualified Control.Concurrent (threadDelay)
import qualified User as User
import qualified Data.ByteString.Char8 as BS

import Network.BSD
import Network (PortID(..))
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo )
import qualified Network.Socket as Socket ( accept )
import qualified Control.Exception as Exception
import Happstack.State.Saver
import Scheduler
import Happstack.State (update,query)
import DocControl
import DocState
import qualified Amazon as AWS

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
  passwdhash <- User.createPassword (BS.pack "admin")
  flip mapM_ User.initialUsers $ \email -> do
      maybeuser <- query $ User.GetUserByEmail email
      case maybeuser of
          Nothing -> do
              update $ User.AddUser BS.empty (User.unEmail email) passwdhash Nothing
              return ()
          Just _ -> return () -- user exist, do not add it
  

uploadOldFilesToAmazon :: AppConf -> IO ()
uploadOldFilesToAmazon appConf = do
  files <- query $ GetFilesThatShouldBeMovedToAmazon
  mapM_ (AWS.uploadFile (defaultAWSAction appConf)) files

main = withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  let progName = "kontrakcja"

  args <- getArgs

  appConf <- case parseConfig args of
    (Left e) -> do logM "Happstack.Server" ERROR (unlines e)
                   exitFailure
    (Right f) -> return (f $ defaultConf progName)
  
  Exception.bracket
                 -- start the state system
              (logM "Happstack.Server" NOTICE ("Using store " ++ store appConf) >>
               startSystemState' (store appConf) stateProxy)
              (\control -> do
                  logM "Happstack.Server" NOTICE "Creating checkpoint before exit" 
                  createCheckpoint control
                  logM "Happstack.Server" NOTICE "Closing transaction system" 
                  shutdownSystem control)
              (\control -> do

                  -- start the http server
                  Exception.bracket 
                           (do 
                              -- we need to use our own listenOn as we want to:
                              -- use only IPv4 addresses
                              -- bind only to 127.0.0.1
                              socket <- listenOn (port (httpConf appConf))
                              t1 <- forkIO $ simpleHTTPWithSocket socket (httpConf appConf) (appHandler appConf)
                              t2 <- forkIO $ cron 60 runScheduler
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
                                          logM "Happstack.Server" NOTICE "Termination request received" 

                  return ())


defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpConf = nullConf
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
              , awsBucket = ""
              , awsSecretKey = ""
              , awsAccessKey = ""
              , production = False
              }

opts :: [OptDescr (AppConf -> AppConf)]
opts = [ Option [] ["http-port"]  
         (ReqArg (\h c -> c { httpConf = (httpConf c) {port = read h} }) "port") 
         "port to bind http server"
       , Option [] ["no-validate"] 
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Nothing } })) 
         "Turn off HTML validation"
       , Option [] ["validate"]    
         (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } })) 
         "Turn on HTML validation"
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

