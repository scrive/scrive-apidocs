{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
module Happstack.Server.Instances where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Happstack.Server

import Control.Monad.Trans.Control.Util
import KontraPrelude

instance (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , FilterMonad f m
  ) => FilterMonad f (t m) where
    setFilter     = lift . setFilter
    composeFilter = lift . composeFilter
    getFilter m   = do
      (stT, f) <- liftWith $ \run -> getFilter (run m)
      (, f) `liftM` restoreT (return stT)

instance (
    Monad m
  , Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , HasRqData m
  ) => HasRqData (t m) where
    askRqEnv       = lift askRqEnv
    localRqEnv f m = controlT $ \run -> localRqEnv f (run m)
    rqDataError    = lift . rqDataError

instance (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , ServerMonad m
  ) => ServerMonad (t m) where
    askRq       = lift askRq
    localRq f m = controlT $ \run -> localRq f (run m)

instance (
    Monad (t m)
  , MonadTrans t
  , WebMonad r m
  ) => WebMonad r (t m) where
    finishWith = lift . finishWith
