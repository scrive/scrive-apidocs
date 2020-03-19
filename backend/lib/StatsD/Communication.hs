module StatsD.Communication
  ( sendStats
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Log
import Network.Socket
import Network.Socket.ByteString
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import StatsD.Config

sendStats
  :: (MonadLog m, MonadBaseControl IO m, TextShow a)
  => StatsDConf
  -> T.Text
  -> [(T.Text, a)]
  -> m ()
sendStats conf namespace nvs = do
  addrinfos <- liftBase
    $ getAddrInfo Nothing (Just $ statsdHost conf) (Just . show $ statsdPort conf)
  case addrinfos of
    []               -> logAttention_ "Can resolve StatsD server address"
    (serveraddr : _) -> liftBase $ E.bracket (open serveraddr) close sendData
  where
    open sa = do
      sock <- socket (addrFamily sa) Datagram defaultProtocol
      connect sock (addrAddress sa)
      return sock
    sendData sock = forM_ nvs $ \(n, v) -> do
      let statName = statsdPrefix conf <> "_" <> namespace <> "." <> n
      liftBase . sendAll sock $ T.encodeUtf8 (statName <> ":" <> showt v <> "|g")
