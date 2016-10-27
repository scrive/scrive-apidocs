module ThirdPartyStats where

import Data.Binary
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import KontraPrelude
import TestKontra
import ThirdPartyStats.Core

thirdPartyStatsTests :: TestEnvSt -> Test
thirdPartyStatsTests _ = testGroup "Third party stats tests" [
    testProperty "Serialization is the inverse of deserialization"
      (\evt -> evt == decode (encode (evt :: AsyncEvent)))
  ]
