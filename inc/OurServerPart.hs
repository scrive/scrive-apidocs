{-# OPTIONS_GHC -fno-warn-orphans #-}
module OurServerPart (
    OurServerPartT
  , runOurServerPartT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Happstack.Server
import Log

import Control.Monad.Trans.Control.Util

instance (MonadLog m) => MonadLog (ServerPartT m) where
  logM a b c = lift $ logM a b c


-- | The purpose of this wrapper is to override ServerPartT implementation
-- of fail in Monad instance, which immediately returns to Happstack (by
-- internal usage of finishWith) and displays ugly "Internal Server Error"
-- page with passed error message (ie. pattern match failure in do notation).
-- this is bad for us (information leakage) and confusing for customers.

newtype OurServerPartT m a = OurServerPartT { runOurServerPartT :: ServerPartT m a }
  deriving (Applicative, FilterMonad Response, Functor, HasRqData, MonadBase b, MonadIO, MonadPlus, MonadTrans, ServerMonad, WebMonad Response, MonadLog)



instance Monad m => Monad (OurServerPartT m) where
  return  = OurServerPartT . return
  m >>= f = OurServerPartT $ runOurServerPartT m >>= runOurServerPartT . f
  -- bypass ServerPartT fail implementation and use the one from m (usually IO)
  fail    = OurServerPartT . lift . fail

instance MonadTransControl OurServerPartT where
  newtype StT OurServerPartT a = StOurServerPartT { unStOurServerPartT :: StT ServerPartT a }
  liftWith = defaultLiftWith OurServerPartT runOurServerPartT StOurServerPartT
  restoreT = defaultRestoreT OurServerPartT unStOurServerPartT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (OurServerPartT m) where
  newtype StM (OurServerPartT m) a = StMOurServerPartT { unStMOurServerPartT :: ComposeSt OurServerPartT m a }
  liftBaseWith = defaultLiftBaseWith StMOurServerPartT
  restoreM     = defaultRestoreM unStMOurServerPartT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}
