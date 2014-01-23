module MessengerServer where

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
import SMS.Tables
import DB.Checks
import DB.PostgreSQL
import MessengerServerConf
import Sender
import Utils.Cron
import Utils.IO
import Utils.Network
import qualified Log

main :: IO ()
main = Log.withLogger $ do
  appname <- getProgName
  conf <- readConfig Log.mixlog_ appname [] "messenger_server.conf"
  checkExecutables

  withPostgreSQL (mscDBConfig conf) $
    checkDatabase Log.mixlog_ messengerTables

  rng <- newCryptoRNGState

  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port }
        (routes, overlaps) = R.compile handlers
    maybe (return ()) Log.mixlog_ overlaps
    socket <- listenOn (htonl iface) $ fromIntegral port
    forkIO $ simpleHTTPWithSocket socket handlerConf (router rng conf routes)
   ) killThread $ \_ -> do
     let sender = createSender $ mscMasterSender conf
         dbconf = mscDBConfig conf
     msender <- newMVar sender
     withCronJobs
       ([ forkCron_ True "SMS Dispatcher" 5 $ dispatcher rng sender msender dbconf
        , forkCron_ True "SMS Cleaner" (60*60) $ cleaner rng dbconf
        ]) $ \_ -> do
            waitForTermination
            Log.mixlog_ $ "Termination request received"
