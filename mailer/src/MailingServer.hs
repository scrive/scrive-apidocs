module MailingServer where

import Control.Concurrent
import Happstack.Server hiding (waitForTermination)
import qualified Control.Exception.Lifted as E
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
import DB
import DB.PostgreSQL
import DB.Checks
import Mails.Tables
import qualified Data.ByteString.Char8 as BS
import qualified MemCache
import qualified Log
import qualified Amazon as AWS

main :: IO ()
main = Log.withLogger $ do
  conf <- readConfig Log.mixlog_ "mailing_server.conf"
  checkExecutables

  let connSource = defaultSource $ defaultSettings { csConnInfo = mscDBConfig conf }
  withPostgreSQL connSource $
    checkDatabase Log.mixlog_ mailerTables

  fcache <- MemCache.new BS.length 52428800
  let amazonconf = AWS.AmazonConfig (mscAmazonConfig conf) fcache
  rng <- newCryptoRNGState

  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
    routes <- case R.compile handlers of
                Left e -> do
                  Log.mixlog_ e
                  error "static routing"
                Right r -> return (r >>= maybe (notFound (toResponse ("Not found."::String))) return)
    socket <- listenOn (htonl iface) $ fromIntegral port
    forkIO $ simpleHTTPWithSocket socket handlerConf (router rng connSource routes)
   ) killThread $ \_ -> do
     let sender = createSender $ mscMasterSender conf
     msender <- newMVar sender
     withCronJobs
       ([ forkCron_ True "Dispatcher" 5 $ dispatcher rng sender msender connSource amazonconf
        , forkCron_ True "Cleaner" (60*60) $ cleaner rng connSource
        ] ++
        case mscSlaveSender conf of
          Just slave -> [ forkCron True "ServiceAvailabilityChecker" 0
                            (serviceAvailabilityChecker conf rng connSource (sender, createSender slave) msender) ]
          Nothing    -> []) $ \_ -> do
      waitForTermination
      Log.mixlog_ $ "Termination request received"
