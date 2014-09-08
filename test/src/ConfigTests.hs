module ConfigTests (configTests) where


import AppConf
import MailingServerConf
import MessengerServerConf
import Data.Unjson
import Utils.Default
import Data.Monoid

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import TestKontra

configTests :: TestEnvSt -> Test
configTests _ = testGroup "Configurations"
    [ testCase "kontrakcja-server config parses" $ testAppConf
    , testCase "mailing-server config parses" $ testMailingServerConf
    , testCase "messenger-server config parses" $ testMessengerServerConf
    ]

testAppConf :: Assertion
testAppConf = do
  let conf = (defaultValue :: AppConf)
      asJson = unjsonToJSON unjsonAppConf conf
      Result conf2 _ = parse unjsonAppConf (Anchored mempty asJson)
  assertEqual "AppConf configuration deserializes properly" conf conf2

testMailingServerConf :: Assertion
testMailingServerConf = do
  let conf = (defaultValue :: MailingServerConf)
      asJson = unjsonToJSON unjsonMailingServerConf conf
      Result conf2 _ = parse unjsonMailingServerConf (Anchored mempty asJson)
  assertEqual "MailingServerConf configuration deserializes properly" conf conf2

testMessengerServerConf :: Assertion
testMessengerServerConf = do
  let conf = (defaultValue :: MessengerServerConf)
      asJson = unjsonToJSON unjsonMessengerServerConf conf
      Result conf2 _ = parse unjsonMessengerServerConf (Anchored mempty asJson)
  assertEqual "MessengerServerConf configuration deserializes properly" conf conf2
