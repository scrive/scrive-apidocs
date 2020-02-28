module SMSLinkShorteningTest (smsLinkShorteningTest) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Doc.SignatoryLinkID
import MagicHash
import TestingUtil ()
import TestKontra
import qualified Util.SMSLinkShortening as SMSLinkShortening

smsLinkShorteningTest :: TestEnvSt -> Test
smsLinkShorteningTest _ = testGroup
  "SMSLinkShorteningTest"
  [testProperty "SMSLinkShorteningTest roundtrip" roundtripTest]

roundtripTest :: (SignatoryLinkID, MagicHash) -> Bool
roundtripTest a = Just a == (SMSLinkShortening.unshort $ SMSLinkShortening.short a)
