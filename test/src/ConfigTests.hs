module ConfigTests (configTests) where


import Data.Unjson
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)

import AppConf
import MailingServerConf
import MessengerServerConf
import TestKontra
import Utils.Default

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
      Result conf2 _ = parse unjsonAppConf asJson
  assertEqual "AppConf configuration deserializes properly" conf conf2

testMailingServerConf :: Assertion
testMailingServerConf = do
  let conf = (defaultValue :: MailingServerConf)
      asJson = unjsonToJSON unjsonMailingServerConf conf
      Result conf2 _ = parse unjsonMailingServerConf asJson
  assertEqual "MailingServerConf configuration deserializes properly" conf conf2

testMessengerServerConf :: Assertion
testMessengerServerConf = do
  let conf = (defaultValue :: MessengerServerConf)
      asJson = unjsonToJSON unjsonMessengerServerConf conf
      Result conf2 _ = parse unjsonMessengerServerConf asJson
  assertEqual "MessengerServerConf configuration deserializes properly" conf conf2
