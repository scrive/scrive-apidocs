{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.TransducerTest where

import Data.Aeson
import Data.Text
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import Flow.Transducer

instance Arbitrary StateId where
  arbitrary = fmap (pack . getASCIIString) arbitrary

instance Arbitrary (TransducerEdge Int Int) where
  arbitrary = TransducerEdge <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TransducerState Int Int) where
  arbitrary = TransducerState <$> arbitrary <*> arbitrary

instance Arbitrary (Transducer Int Int) where
  arbitrary = Transducer <$> arbitrary <*> arbitrary <*> arbitrary

tests :: Test
tests = testGroup
  "Transducer"
  [ testProperty "Tranducer JSON serialization deserialization"
                 transducerJSONSerializationDesereialization
  ]

transducerJSONSerializationDesereialization :: Transducer Int Int -> Bool
transducerJSONSerializationDesereialization transducer =
  isJust . decode @(Transducer Int Int) $ encode transducer

