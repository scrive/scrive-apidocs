{-# LANGUAGE OverlappingInstances #-}
module Data.Time.Monad where

import Control.Monad.Trans
import Data.Time
import Prelude

-- | Class of monads which carry the notion of the current time.
class Monad m => MonadTime m where
  currentTime :: m UTCTime

instance MonadTime IO where
  currentTime = getCurrentTime

-- | Generic, overlapping instance.
instance (
    MonadTime m
  , MonadTrans t
  , Monad (t m)
  ) => MonadTime (t m) where
    currentTime = lift currentTime
