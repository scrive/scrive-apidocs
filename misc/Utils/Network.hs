{-# LANGUAGE ForeignFunctionInterface #-}
module Utils.Network where

import Data.Word
import Network.BSD
import Network.Socket
import qualified Control.Exception as E

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32

-- | Network.listenOn bind randomly to IPv4 or IPv6 or both,
-- depending on system and local settings.
-- Lets make it use IPv4 only
listenOn :: HostAddress -> PortNumber -> IO Socket
listenOn iface port = do
  proto <- getProtocolNumber "tcp"
  E.bracketOnError
    (socket AF_INET Stream proto)
    sClose
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bindSocket sock (SockAddrInet port iface)
      listen sock maxListenQueue
      return sock
     )
