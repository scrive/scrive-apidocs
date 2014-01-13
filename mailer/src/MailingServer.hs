module MailingServer where

import Control.Concurrent
import System.Environment
import Happstack.Server hiding (waitForTermination)
import qualified Control.Exception as E
import qualified Happstack.StaticRouting as R

import Cleaner
import Crypto.RNG (newCryptoRNGState)
import Configuration
import Dispatcher
import Handlers
import MailingServerConf
import Sender
import ServiceChecker
import Utils.Cron
import Utils.IO
import Utils.Network
import DB.PostgreSQL
import DB.Checks
import Mails.Tables
import qualified Data.ByteString.Char8 as BS
import qualified MemCache
import qualified Log (withLogger, mailingServer)
import qualified Amazon as AWS

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig Log.mailingServer appname [] "mailing_server.conf"
  checkExecutables
  withPostgreSQL (mscDBConfig conf) $
    checkDatabase Log.mailingServer mailerTables

  fcache <- MemCache.new BS.length 52428800
  let amazonconf = AWS.AmazonConfig (mscAmazonConfig conf) fcache
  rng <- newCryptoRNGState

  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        (routes, overlaps) = R.compile handlers
    maybe (return ()) Log.mailingServer overlaps
    socket <- listenOn (htonl iface) $ fromIntegral port
    forkIO $ simpleHTTPWithSocket socket handlerConf (router rng conf routes)
   ) killThread $ \_ -> do
     let sender = createSender $ mscMasterSender conf
         dbconf = mscDBConfig conf
     msender <- newMVar sender
     withCronJobs
       ([ forkCron_ True "Dispatcher" 5 $ dispatcher rng sender msender dbconf amazonconf
        , forkCron_ True "Cleaner" (60*60) $ cleaner rng dbconf
        ] ++
        case mscSlaveSender conf of
          Just slave -> [ forkCron True "ServiceAvailabilityChecker" 0
                            (serviceAvailabilityChecker conf rng dbconf (sender, createSender slave) msender) ]
          Nothing    -> []) $ \_ -> do
      waitForTermination
      Log.mailingServer $ "Termination request received"
