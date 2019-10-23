module StatsD.Communication (
  sendstats
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Log
import Network.Socket
import Network.Socket.ByteString
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as BS

import StatsD.Config

sendstats
  :: (MonadLog m, MonadIO m, MonadBaseControl IO m)
  => StatsDConf
  -> String
  -> [(String, Int)]
  -> m ()
sendstats conf namespace nvs = do
  addrinfos <- liftIO
    $ getAddrInfo Nothing (Just $ statsdHost conf) (Just $ show $ statsdPort conf)
  case (addrinfos) of
    []               -> logAttention_ "Can resolve StatsD server address"
    (serveraddr : _) -> liftIO $ E.bracket (open serveraddr) close sendData
  where
    open sa = do
      sock <- socket (addrFamily sa) Datagram defaultProtocol
      connect sock (addrAddress sa)
      return sock
    sendData sock = forM_ nvs $ \(n, v) -> do
      let statName = (statsdPrefix conf) ++ "_" ++ namespace ++ "." ++ n
      liftIO $ sendAll sock $ BS.fromString $ statName ++ ":" ++ show v ++ "|g"
