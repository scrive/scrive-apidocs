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

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Traversable (sequenceA)

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

-- | 'sequenceA' says that if we maybe have @(Maybe (m a))@ a computation
-- that gives a then we can get real computation that may fail m
-- @(Maybe a)@ 'sequenceMM' does the same, but is aware that first
-- computation can also fail, and so it joins two posible fails.
sequenceMM :: (Applicative m) => Maybe (m (Maybe a)) -> m (Maybe a)
sequenceMM = (fmap join) . sequenceA

{- |
   Applies the first argument to the second argument if the second
   argument returns Just. Otherwise, returns nothing.
 -}
liftMM ::(Monad m) => (a -> m (Maybe b)) -> m (Maybe a) -> m (Maybe b)
liftMM f v = do
  mv <- v
  maybe (return Nothing) f mv

lift_M ::(Monad m) => (a -> m b) -> m (Maybe a) -> m (Maybe b)
lift_M f v = do
  mv <- v
  maybe (return Nothing) (liftM Just . f) mv

{- |
   like when but always returns ()
 -}
when_ :: (Monad m) => Bool -> m a -> m ()
when_ b c = when b $ c >> return ()

