{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StrictData #-}

module Flow.OrphanTestInstances () where

import Data.String
import Data.Text
import Test.QuickCheck

import Flow.Message.Internal
import Flow.Names.Internal
import Flow.Process.Internal

deriving instance IsString Message
deriving instance IsString (Name a)
deriving instance IsString Process

instance Arbitrary (Name a) where
  arbitrary = fmap (Name . pack . getASCIIString) arbitrary
