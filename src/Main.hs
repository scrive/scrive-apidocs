module Main where

import Control.Concurrent (MVar, forkIO, killThread)
import Happstack.Util.Cron (cron)
import Happstack.State (waitForTermination)
import Happstack.Server
  ( Conf(port)
  , simpleHTTP
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
  , Saver(Queue, FileSaver)
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
import AppControl (appHandler)
import qualified System.Mem (performGC)
import qualified Control.Concurrent (threadDelay)

import Network.BSD
import Network (PortID(..))
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo )
import qualified Network.Socket as Socket ( accept )
import qualified Control.Exception as Exception
import Network.Curl

import Session
import Happstack.State (update)

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
            bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )

main = withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  let progName = "kontrakcja"

  args <- getArgs

  appConf <- case parseConfig args of
    (Left e) -> do logM progName ERROR (unlines e)
                   exitFailure
    (Right f) -> return (f $ defaultConf progName)
  
  withCurlDo $ Exception.bracket
                 -- start the state system
              (startSystemState' (store appConf) stateProxy)
              (\control -> do
                  logM "Happstack.Server" NOTICE "Creating checkpoint before exit" 
                  createCheckpoint control
                  logM "Happstack.Server" NOTICE "Closing transaction system" 
                  shutdownSystem control)
              (\control -> do

                  -- start the http server
                  Exception.bracket 
                           (do -- socket <- listenOn (port (httpConf appConf))
                               -- forkIO $ simpleHTTPWithSocket socket (httpConf appConf) appHandler
                             forkIO $ simpleHTTP (httpConf appConf) appHandler)
                           (killThread) $ \_ -> Exception.bracket
                                        -- checkpoint the state once a day
                                        -- FIXME: make it checkpoint always at the same time
                                        (forkIO $ cron (60*60*24) (createCheckpoint control))
                                        (killThread) $ \_ -> do

                                          -- wait for termination signal
                                          waitForTermination
                                          logM "Happstack.Server" NOTICE "Termination request received" 

                  return ())

data AppConf
    = AppConf { httpConf :: Conf
              , store :: FilePath
              , static :: FilePath 
              }

defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpConf = nullConf
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
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
       ]

parseConfig :: [String] -> Either [String] (AppConf -> AppConf)
parseConfig args
    = case getOpt Permute opts args of
        (flags,_,[]) -> Right $ \appConf -> foldr ($) appConf flags
        (_,_,errs)   -> Left errs

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' = runTxSystem . Queue . FileSaver

