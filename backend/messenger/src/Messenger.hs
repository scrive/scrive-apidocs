module Messenger (
    Messenger
  , runMessenger
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Happstack.Server
import Log

import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler
import KontraPrelude

type InnerMessenger = CryptoRNGT (DBT (ReqHandlerT (LogT IO)))

newtype Messenger a = Messenger { unMessenger :: InnerMessenger a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, MonadTime, ServerMonad, MonadLog)

instance MonadBaseControl IO Messenger where
  type StM Messenger a = StM InnerMessenger a
  liftBaseWith f = Messenger $ liftBaseWith $ \run -> f $ run . unMessenger
  restoreM       = Messenger . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMessenger :: CryptoRNGState -> Messenger a -> DBT (ReqHandlerT (LogT IO)) a
runMessenger rng = runCryptoRNGT rng . unMessenger
