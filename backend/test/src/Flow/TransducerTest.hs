{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.TransducerTest where

import Data.Aeson
import Data.Set (Set)
import Data.Text
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

import Flow.HighTongue
import Flow.Machinize
import Flow.Transducer

instance Arbitrary StateId where
  arbitrary = fmap (pack . getASCIIString) arbitrary

-- Transducer Int Int

instance Arbitrary (TransducerEdge Int Int) where
  arbitrary = TransducerEdge <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TransducerState Int Int) where
  arbitrary = TransducerState <$> arbitrary <*> arbitrary

instance Arbitrary (Transducer Int Int) where
  arbitrary = Transducer <$> arbitrary <*> arbitrary <*> arbitrary

transducerIntJSONSerializeDeserialize :: Transducer Int Int -> Bool
transducerIntJSONSerializeDeserialize transducer =
  isJust . decode @(Transducer Int Int) $ encode transducer

-- Transducer (Set EventInfo) [LowAction]

instance Arbitrary SystemAction where
  arbitrary = oneof [Notify <$> arbitrary <*> arbitrary, Close <$> arbitrary]

instance Arbitrary LowAction where
  arbitrary = oneof [Action <$> arbitrary, pure Fail]

instance Arbitrary UserAction where
  arbitrary = elements [Field "somefield", Approval, Signature, View, Rejection, Timeout]

instance Arbitrary EventInfo where
  arbitrary = EventInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TransducerEdge (Set EventInfo) [LowAction]) where
  arbitrary = TransducerEdge <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TransducerState (Set EventInfo) [LowAction]) where
  arbitrary = TransducerState <$> arbitrary <*> arbitrary

instance Arbitrary (Transducer (Set EventInfo) [LowAction]) where
  arbitrary = Transducer <$> arbitrary <*> arbitrary <*> arbitrary

-- Test group

tests :: Test
tests = testGroup
  "Transducer"
  [ testProperty "Transducer Int Int: JSON serialization deserialization"
                 transducerIntJSONSerializeDeserialize
  ]




