-----------------------------------------------------------------------------
-- |
-- Module      :  Util.MonadUtils
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utilities for working succinctly with monads.
-----------------------------------------------------------------------------
module Util.MonadUtils where

import Control.Monad.Base
import Control.Monad.Trans

import KontraError
import qualified Log

{- |
   Get the value from a Just or fail if it is Nothing
 -}
guardJust :: MonadBase IO m => Maybe a -> m a
guardJust = maybe internalError return

{- |
   Get the value from a Just or fail if it is Nothing
 -}
guardJustM :: MonadBase IO m => m (Maybe b) -> m b
guardJustM action = guardJust =<< action


{- |
   Get the value from a Right or log an error and fail if it is Left
 -}
guardRight :: (MonadBase IO m, MonadIO m, Show msg, Log.MonadLog m) => Either msg a -> m a
guardRight (Right val) = return val
guardRight (Left  msg) = do
  Log.mixlog_ (show msg)
  internalError

{- |
   Get the value from a Right or log an error and fail if it is a left
 -}
guardRightM :: (MonadBase IO m, MonadIO m, Show msg, Log.MonadLog m) => m (Either msg b) -> m b
guardRightM action = guardRight =<< action
