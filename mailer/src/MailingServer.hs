{-# LANGUAGE CPP #-}
module MailingServer where

import Control.Concurrent
import System.Environment
import Happstack.Server (simpleHTTPWithSocket, nullConf, port)
import qualified Happstack.StaticRouting as R
import qualified Control.Exception as E

import Cleaner
import Crypto.RNG (newCryptoRNGState)
import Configuration
import Dispatcher
import DB.Checks
import DB.PostgreSQL
import Handlers
import Mails.Migrations
import Mails.Tables
import MailingServerConf
import Sender
import ServiceChecker
import Utils.IO
import Utils.Network
import qualified Log (withLogger, mailingServer)

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig Log.mailingServer appname [] "mailing_server.conf"
  rng <- newCryptoRNGState
  withPostgreSQL (mscDBConfig conf) $
    performDBChecks Log.mailingServer mailerTables mailerMigrations
  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        sender = createSender $ mscMasterSender conf
        (routes, overlaps) = R.compile handlers
        dbconf = mscDBConfig conf
    msender <- newMVar sender
    maybe (return ()) Log.mailingServer overlaps
    socket <- listenOn (htonl iface) $ fromIntegral port
    t1 <- forkIO $ simpleHTTPWithSocket socket handlerConf (router rng conf routes)
    t2 <- forkIO $ dispatcher rng sender msender dbconf
    t3 <- forkIO $ cleaner rng dbconf
    t4 <- case mscSlaveSender conf of
      Just slave -> return `fmap` (forkIO $ serviceAvailabilityChecker rng dbconf (sender, createSender slave) msender)
      Nothing -> return []
    return $ t1 : t2 : t3 : t4
   ) (mapM_ killThread) (\_ -> do
     waitForTermination
     Log.mailingServer $ "Termination request received"
   )
