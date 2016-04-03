{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
module Control.Monad.Trans.Instances () where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control

import Control.Monad.Trans.Control.Util
import KontraPrelude

instance (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , MonadReader r m
  ) => MonadReader r (t m) where
    ask       = lift ask
    local f m = controlT $ \run -> local f (run m)

instance (
    Monad (t m)
  , MonadTrans t
  , MonadState s m
  ) => MonadState s (t m) where
    get = lift get
    put = lift . put

{-

-- This is wrong: there exists monad transformer t for which
-- t m doesn't have a valid instance of MonadMask even though
-- m does (an example is EitherT or MaybeT). Unfortunately it
-- doesn't prevent you from creating them as MonadBaseControl
-- does.

instance (
    Monad (t m)
  , MonadTransControl t
  , MonadMask m
  ) => MonadMask (t m) where
    mask = liftMask mask
    uninterruptibleMask = liftMask uninterruptibleMask

-}
