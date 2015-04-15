module Messenger (
    Messenger
  , runMessenger
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes.Class.Instances.Overlapping ()
import Happstack.Server

import Control.Monad.Trans.Control.Util
import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import Happstack.Server.Instances ()
import KontraPrelude
import MinutesTime.Class
import qualified Log

type InnerMessenger = CryptoRNGT (DBT (ServerPartT (Log.LogT IO)))

newtype Messenger a = Messenger { unMessenger :: InnerMessenger a }
  deriving (Alternative, Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, MonadTime, ServerMonad, Log.MonadLog)

instance MonadBaseControl IO Messenger where
  newtype StM Messenger a = StMessenger { unStMessenger :: StM InnerMessenger a }
  liftBaseWith = newtypeLiftBaseWith Messenger unMessenger StMessenger
  restoreM     = newtypeRestoreM Messenger unStMessenger
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMessenger :: CryptoRNGState -> Messenger a -> DBT (ServerPartT (Log.LogT IO)) a
runMessenger rng = runCryptoRNGT rng . unMessenger
