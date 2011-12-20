{-# LANGUAGE CPP #-}
module MailingServer where

import Control.Concurrent
import Control.Monad
import System.Environment
import Happstack.Server
import Happstack.State
import qualified Control.Exception as E

import Configuration
import MailingServerConf
import Network
import qualified AppLogger as Log

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig appname [] "mailing_server.conf"
  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
    socket <- listenOn (htonl iface) $ fromIntegral port
    t1 <- forkIO $ simpleHTTPWithSocket socket handlerConf mailerHandler
    return [t1]
   ) (mapM_ killThread) (\_ -> do
     waitForTermination
     Log.server $ "Termination request received"
   )

mailerHandler :: ServerPartT IO String
mailerHandler = return "Dummy handler"
