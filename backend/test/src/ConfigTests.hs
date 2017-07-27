module ConfigTests (configTests) where

import Data.Default
import Data.Proxy
import Data.Unjson
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import AppConf
import Configuration
import CronConf
import KontraPrelude
import MailingServerConf
import MessengerServerConf
import TestKontra

configTests :: TestEnvSt -> Test
configTests _ = testGroup "Configurations"
    [ testCase "kontrakcja-server config parses" $
      testConfRoundtrip "AppConf"             unjsonAppConf

    , testCase "cron config parses"              $
      testConfRoundtrip "CronConf"            unjsonCronConf

    , testCase "mailing-server config parses"    $
      testConfRoundtrip "MailingServerConf"   unjsonMailingServerConf

    , testCase "messenger-server config parses"  $
      testConfRoundtrip "MessengerServerConf" unjsonMessengerServerConf

    , testCase "kontrakcja-server config template parses" $
      testConfTemplate "configuration-templates/kontrakcja.conf.template"
      (Proxy :: Proxy AppConf)

    , testCase "cron config template parses"              $
      testConfTemplate "configuration-templates/cron.conf.template"
      (Proxy :: Proxy CronConf)

    , testCase "mailing-server config template parses"    $
      testConfTemplate "configuration-templates/mailing_server.conf.template"
      (Proxy :: Proxy MailingServerConf)

    , testCase "messenger-server config template parses"  $
      testConfTemplate "configuration-templates/messenger_server.conf.template"
      (Proxy :: Proxy MessengerServerConf)

    , testCase "kontrakcja-server vagrant config template parses" $
      testConfTemplate "vagrant/kontrakcja.conf"
      (Proxy :: Proxy AppConf)

    , testCase "mailing-server vagrant config template parses"    $
      testConfTemplate "vagrant/mailing_server.conf"
      (Proxy :: Proxy MailingServerConf)

    , testCase "messenger-server vagrant config template parses"  $
      testConfTemplate "vagrant/messenger_server.conf"
      (Proxy :: Proxy MessengerServerConf)

    ]

testConfRoundtrip :: forall a . (Eq a, Show a, Default a) =>
                     String -> UnjsonDef a -> Assertion
testConfRoundtrip confName unjsonConf = do
  let conf           = (def :: a)
      asJson         = unjsonToJSON unjsonConf conf
      Result conf2 _ = parse unjsonConf asJson
  assertEqual (confName ++ " configuration deserializes properly") conf conf2

testConfTemplate :: forall a . (Eq a, Show a, Default a, Unjson a) =>
                  FilePath -> Proxy a -> Assertion
testConfTemplate confPath _proxy = do
  -- Throws an error when the config doesn't parse, for example:
  --
  -- kontrakcja-server config template parses: [Failed]
  -- ERROR: user error (There were issues with the content of
  -- configuration kontrakcja.conf.template)

  let suppressLog = const $ return () -- replace with putStrLn for debugging
  (_ :: a) <- readConfigEx suppressLog confPath
               (def { optReadConfigUncommentKeys = True })
  (_ :: a) <- readConfigEx suppressLog confPath
               (def { optReadConfigUncommentKeys = False })
  return ()
