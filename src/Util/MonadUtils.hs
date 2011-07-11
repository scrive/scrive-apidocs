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

import Control.Monad
import Control.Monad.Trans

import qualified AppLogger as Log

{- |
   Get the value from a Just or mzero if it is Nothing
 -}
guardJust :: (MonadPlus m) => Maybe a -> m a
guardJust = maybe mzero return

{- |
   Get the value from a Just or mzero if it is Nothing
 -}
guardJustM :: (MonadPlus m) => m (Maybe b) -> m b
guardJustM action = guardJust =<< action

{- |
   Get the value from a Right or log an error and mzero if it is Left
 -}
guardRight :: (MonadPlus m, Monad m, MonadIO m, Show msg) => Either msg a -> m a
guardRight (Right val) = return val
guardRight (Left  msg) = do 
  Log.debug (show msg)
  mzero
  
{- |
   Get the value from a Right or log an error and mzero if it is a left
 -}
guardRightM :: (MonadPlus m, MonadIO m, Show msg) => m (Either msg b) -> m b
guardRightM action = guardRight =<< action
