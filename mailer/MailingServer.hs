{-# LANGUAGE CPP #-}
module MailingServer where

import Control.Concurrent
import Control.Monad
import Database.HDBC.PostgreSQL
import System.Environment
import Happstack.Server
import Happstack.State
import qualified Control.Exception as E

import Configuration
import DB.Classes
import DB.Checks
import Mailer
import Mails.Tables
import MailingServerConf
import Network
import Sender
import qualified AppLogger as Log (withLogger, mailingServer)

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig Log.mailingServer appname [] "mailing_server.conf"
  withPostgreSQL (mscDBConfig conf) $ \conn ->
    ioRunDB conn $ performDBChecks Log.mailingServer [tableMails] []
  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        mailer = createMailer $ mscMailsConfig conf
    socket <- listenOn (htonl iface) $ fromIntegral port
    t1 <- forkIO $ simpleHTTPWithSocket socket handlerConf mailerHandler
    t2 <- forkIO $ sender mailer $ mscDBConfig conf
    return [t1, t2]
   ) (mapM_ killThread) (\_ -> do
     waitForTermination
     Log.mailingServer $ "Termination request received"
   )

mailerHandler :: ServerPartT IO String
mailerHandler = return "Dummy handler"
