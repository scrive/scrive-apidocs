module ThirdPartyStats where
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import ThirdPartyStats.Core
import TestKontra
import Data.Binary

thirdPartyStatsTests :: TestEnvSt -> Test
thirdPartyStatsTests _ = testGroup "Third party stats tests" [
    testProperty "Serialization is the inverse of deserialization"
      (\evt -> evt == decode (encode (evt :: AsyncEvent)))
  ]