{-# LANGUAGE CPP #-}
module MailingServer where

import Control.Concurrent
import DB.Nexus
import Database.HDBC.PostgreSQL
import System.Environment
import Happstack.Server
import Happstack.State
import qualified Happstack.StaticRouting as R
import qualified Control.Exception as E

import Configuration
import Dispatcher
import DB.Classes
import DB.Checks
import Handlers
import Mails.Migrations
import Mails.Tables
import MailingServerConf
import Network
import Sender
import qualified Log (withLogger, mailingServer)

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig Log.mailingServer appname [] "mailing_server.conf"
  withPostgreSQL (mscDBConfig conf) $ \conn' -> do
    conn <- mkNexus conn'
    ioRunDB conn $ performDBChecks Log.mailingServer mailerTables mailerMigrations
  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        sender = createSender $ mscMailsConfig conf
        (routes, overlaps) = R.compile handlers
    maybe (return ()) Log.mailingServer overlaps
    socket <- listenOn (htonl iface) $ fromIntegral port
    t1 <- forkIO $ simpleHTTPWithSocket socket handlerConf (router conf routes)
    t2 <- forkIO $ dispatcher sender $ mscDBConfig conf
    return [t1, t2]
   ) (mapM_ killThread) (\_ -> do
     waitForTermination
     Log.mailingServer $ "Termination request received"
   )
