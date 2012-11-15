{-# LANGUAGE CPP #-}
module MailingServer where

import Control.Applicative
import Control.Concurrent
import System.Environment
import Happstack.Server hiding (waitForTermination)
import qualified Control.Concurrent.Thread.Group as TG
import qualified Control.Exception as E
import qualified Happstack.StaticRouting as R

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
import Utils.Cron
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
    tg <- TG.new
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        sender = createSender $ mscMasterSender conf
        (routes, overlaps) = R.compile handlers
        dbconf = mscDBConfig conf
    msender <- newMVar sender
    maybe (return ()) Log.mailingServer overlaps
    socket <- listenOn (htonl iface) $ fromIntegral port
    srvr <- forkIO $ simpleHTTPWithSocket socket handlerConf (router rng conf routes)
    t1 <- forkCron_ tg "Dispatcher" 5 $ dispatcher rng sender msender dbconf
    t2 <- forkCron_ tg "Cleaner" (60*60*24) $ cleaner rng dbconf
    t3 <- case mscSlaveSender conf of
      Just slave -> return <$> forkCron tg "ServiceAvailabilityChecker" 0
        (serviceAvailabilityChecker rng dbconf (sender, createSender slave) msender)
      Nothing -> return []
    return (tg, srvr, t1:t2:t3)
   ) killThreads (\_ -> do
     waitForTermination
     Log.mailingServer $ "Termination request received"
   )
  where
    killThreads (tg, srvr, threads) = do
      Log.mailingServer "Waiting for subthreads to finish..."
      killThread srvr
      mapM_ stopCron threads
      TG.wait tg
