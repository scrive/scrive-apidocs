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

import KontraError

-- TODO: remove these functions.

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
