{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
module ConfigTests (configTests) where

import Data.Unjson
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import AppConf
import Configuration
import CronConf
import MailingServerConf
import MessengerServerConf
import TestKontra

configTests :: TestEnvSt -> Test
configTests _ = testGroup
  "Configurations"
  [ testCase "kontrakcja-server config parses" $ testConfRoundtrip @AppConf
    "AppConf"
    "configuration-templates/kontrakcja.conf.template"
  , testCase "cron config parses"
    $ testConfRoundtrip @CronConf "CronConf" "configuration-templates/cron.conf.template"
  , testCase "mailing-server config parses" $ testConfRoundtrip @MailingServerConf
    "MailingServerConf"
    "configuration-templates/mailing_server.conf.template"
  , testCase "messenger-server config parses" $ testConfRoundtrip @MessengerServerConf
    "MessengerServerConf"
    "configuration-templates/messenger_server.conf.template"
  , testCase "kontrakcja-server config template parses"
    $ testConfTemplate @AppConf "configuration-templates/kontrakcja.conf.template"
  , testCase "cron config template parses"
    $ testConfTemplate @CronConf "configuration-templates/cron.conf.template"
  , testCase "mailing-server config template parses" $ testConfTemplate @MailingServerConf
    "configuration-templates/mailing_server.conf.template"
  , testCase "messenger-server config template parses"
    $ testConfTemplate @MessengerServerConf
        "configuration-templates/messenger_server.conf.template"
  , testCase "kontrakcja-server vagrant config template parses"
    $ testConfTemplate @AppConf "vagrant/kontrakcja.conf"
  , testCase "cron vagrant config template parses"
    $ testConfTemplate @CronConf "vagrant/cron.conf"
  , testCase "mailing-server vagrant config template parses"
    $ testConfTemplate @MailingServerConf "vagrant/mailing_server.conf"
  , testCase "messenger-server vagrant config template parses"
    $ testConfTemplate @MessengerServerConf "vagrant/messenger_server.conf"
  ]

testConfRoundtrip
  :: forall  a . (Eq a, Show a, Unjson a) => String -> FilePath -> Assertion
testConfRoundtrip confName confPath = do
  let suppressLog = const $ return () -- replace with putStrLn for debugging
  (conf :: a) <- readConfigEx suppressLog
                              confPath
                              (ReadConfigOptions { optReadConfigUncommentKeys = True })
  let asJson         = unjsonToJSON unjsonDef conf
      Result conf2 _ = parse unjsonDef asJson
  assertEqual (confName ++ " configuration deserializes properly") conf conf2

testConfTemplate :: forall  a . (Eq a, Show a, Unjson a) => FilePath -> Assertion
testConfTemplate confPath = do
  -- Throws an error when the config doesn't parse, for example:
  --
  -- kontrakcja-server config template parses: [Failed]
  -- ERROR: user error (There were issues with the content of
  -- configuration kontrakcja.conf.template)

  let suppressLog = const $ return () -- replace with putStrLn for debugging
  (_ :: a) <- readConfigEx suppressLog
                           confPath
                           (ReadConfigOptions { optReadConfigUncommentKeys = True })
  (_ :: a) <- readConfigEx suppressLog
                           confPath
                           (ReadConfigOptions { optReadConfigUncommentKeys = False })
  return ()
