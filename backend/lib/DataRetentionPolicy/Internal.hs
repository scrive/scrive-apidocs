{-# LANGUAGE TemplateHaskell #-}
module DataRetentionPolicy.Internal
  ( DataRetentionPolicy(..)
  ) where

import Data.Int
import Optics.TH

data DataRetentionPolicy = DataRetentionPolicy
  { idleDocTimeoutPreparation :: Maybe Int16
  , idleDocTimeoutClosed      :: Maybe Int16
  , idleDocTimeoutCanceled    :: Maybe Int16
  , idleDocTimeoutTimedout    :: Maybe Int16
  , idleDocTimeoutRejected    :: Maybe Int16
  , idleDocTimeoutError       :: Maybe Int16
  , immediateTrash            :: Bool
  } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''DataRetentionPolicy
