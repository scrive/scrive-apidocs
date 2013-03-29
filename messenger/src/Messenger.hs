module Messenger (
    Messenger
  , runMessenger
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Happstack.Server

import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB.Core
import qualified Log
import OurServerPart ()

type InnerMessenger = CryptoRNGT (DBT (ServerPartT IO))

newtype Messenger a = Messenger { unMessenger :: InnerMessenger a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadDB, MonadIO, MonadPlus, ServerMonad, Log.MonadLog)

instance MonadBaseControl IO Messenger where
  newtype StM Messenger a = StMessenger { unStMessenger :: StM InnerMessenger a }
  liftBaseWith = newtypeLiftBaseWith Messenger unMessenger StMessenger
  restoreM     = newtypeRestoreM Messenger unStMessenger
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMessenger :: CryptoRNGState -> Messenger a -> DBT (ServerPartT IO) a
runMessenger rng = runCryptoRNGT rng . unMessenger
