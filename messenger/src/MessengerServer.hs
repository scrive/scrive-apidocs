module MessengerServer where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Happstack.Server hiding (waitForTermination)
import qualified Control.Exception.Lifted as E
import qualified Happstack.StaticRouting as R

import Cleaner
import Configuration
import Crypto.RNG (newCryptoRNGState)
import DB
import DB.Checks
import DB.PostgreSQL
import Dispatcher
import Handlers
import MessengerServerConf
import Sender
import SMS.Tables
import Utils.Cron
import Utils.IO
import Utils.Network
import qualified Log

main :: IO ()
main = Log.withLogger $ do
  conf <- readConfig Log.mixlog_ "messenger_server.conf"
  checkExecutables

  let connSource = simpleSource $ def { csConnInfo = mscDBConfig conf }
  withPostgreSQL connSource $
    checkDatabase Log.mixlog_ [] messengerTables

  rng <- newCryptoRNGState

  E.bracket (do
    let (iface, port) = mscHttpBindAddress conf
        handlerConf = nullConf { port = fromIntegral port, logAccess = Nothing }
    routes <- case R.compile handlers of
      Left e -> do
        Log.mixlog_ e
        error "static routing"
      Right r -> return (r >>= maybe (notFound (toResponse ("Not found."::String))) return)
    socket <- liftBase (listenOn (htonl iface) $ fromIntegral port)
    fork $ (liftBaseWith $ \_runInBase -> simpleHTTPWithSocket socket handlerConf (mapServerPartT Log.withLogger (router rng connSource routes)))
   ) (liftBase killThread) $ \_ -> do
     let sender = createSender $ mscMasterSender conf
     msender <- newMVar sender
     withCronJobs
       ([ forkCron_ True "SMS Dispatcher" 5 $ dispatcher rng sender msender connSource
        , forkCron_ True "SMS Cleaner" (60*60) $ cleaner rng connSource
        ]) $ \_ -> do
            liftBase waitForTermination
            Log.mixlog_ $ "Termination request received"
