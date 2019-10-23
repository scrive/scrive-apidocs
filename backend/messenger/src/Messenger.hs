module Messenger (
    Messenger
  , runMessenger
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Happstack.Server
import Log

import Control.Monad.Trans.Instances ()
import DB
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler

type InnerMessenger = CryptoRNGT (DBT (LogT (ReqHandlerT IO)))

newtype Messenger a = Messenger { unMessenger :: InnerMessenger a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData
           ,Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask
           ,MonadThrow, MonadTime, ServerMonad, MonadLog)

instance MonadBaseControl IO Messenger where
  type StM Messenger a = StM InnerMessenger a
  liftBaseWith f = Messenger $ liftBaseWith $ \run -> f $ run . unMessenger
  restoreM = Messenger . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMessenger :: CryptoRNGState -> Messenger a -> DBT (LogT (ReqHandlerT IO)) a
runMessenger rng = runCryptoRNGT rng . unMessenger
