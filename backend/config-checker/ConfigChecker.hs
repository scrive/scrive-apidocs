{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.Default
import Data.Either.Validation
import Data.Proxy
import Data.Unjson
import System.Environment
import System.Exit
import System.IO

import AppConf
import Configuration
import CronConf
import KontraPrelude
import MailingServerConf
import MessengerServerConf

printHelpMessage :: String -> IO ()
printHelpMessage prog = putStr . unlines $
  [ "Usage: "
  , ""
  , prog <> " def   - Generate default config files."
  , prog <> " check - Check that config files are in sync."
  ]

-- | Map between parsed configuration types (like 'AppConf') and file names.
class ConfigFile config where
  -- | Given a parsed configuration type, return the config file name.
  configFile :: FilePath

readConfigFile :: forall config .
                  (ConfigFile config, Default config, Unjson config)
               => IO config
readConfigFile = readConfig putStrLn (configFile @ config)

instance ConfigFile AppConf where
  configFile = "kontrakcja.conf"

instance ConfigFile CronConf where
  configFile = "cron.conf"

instance ConfigFile MailingServerConf where
  configFile = "mailing_server.conf"

instance ConfigFile MessengerServerConf where
  configFile = "messenger_server.conf"

lintConfigFiles :: IO ()
lintConfigFiles = do
  appConf       <- readConfigFile @AppConf
  cronConf      <- readConfigFile @CronConf
  mailerConf    <- readConfigFile @MailingServerConf
  messengerConf <- readConfigFile @MessengerServerConf

  putStrLn "Checking that config files are in sync..."

  case checkFieldsEqualAppConfCronConf      appConf cronConf      *>
       checkFieldsEqualAppConfMailerConf    appConf mailerConf    *>
       checkFieldsEqualAppConfMessengerConf appConf messengerConf of
    Success ()   -> putStrLn "Checks finished."
    Failure errs -> do
      forM_ errs $ hPutStrLn stderr
      exitFailure

type ConfigValidation = Validation [String] ()

checkFieldEq :: Eq a
             => (FilePath, FilePath) -> String -> a -> a -> ConfigValidation
checkFieldEq (conf0, conf1) fieldName a b =
  if a == b then Success ()
  else Failure [ "Field '" <> fieldName <> "' in '" <> conf0 <> "' and '" <>
                 conf1 <> "' is not in sync." ]

checkFieldsEqualAppConfCronConf :: AppConf -> CronConf
                                -> ConfigValidation
checkFieldsEqualAppConfCronConf
   -- Verbose pattern matching here is deliberate, so that we get a
   -- warning when a new field is added.
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps           amazonConfig
     dbConfig           _maxDBConnections    redisCacheConfig  _localFileCacheSize
    _logConfig          _production         _cdnBaseUrl         guardTimeConf
    _isMailBackdoorOpen  mailNoreplyAddress  cgiGrpConfig      _admins
    _sales              _initialUsers        mixpanelToken     _trackjsToken
    _hubspotConf         salesforceConf     _netsConfig        _monitoringConfig)
  (CronConf
     cronAmazonConfig              cronDBConfig                   _cronMaxDBConnections
     cronRedisCacheConfig         _cronLocalFileCacheSize         _cronLogConfig
     cronGuardTimeConf             cronCgiGrpConfig                cronMixpanelToken
    _cronNtpServers                cronSalesforceConf             _cronInvoicingSFTPConf
    _cronPlanhatConf              _cronMonitoringConf              cronMailNoreplyAddress
    _cronConsumerCronMaxJobs      _cronConsumerSealingMaxJobs     _cronConsumerSigningMaxJobs
    _cronConsumerExtendingMaxJobs _cronConsumerAPICallbackMaxJobs)

  = checkEq "amazon"               amazonConfig       cronAmazonConfig       *>
    checkEq "database"             dbConfig           cronDBConfig           *>
    checkEq "redis_cache"          redisCacheConfig   cronRedisCacheConfig   *>
    checkEq "guardtime"            guardTimeConf      cronGuardTimeConf      *>
    checkEq "mail_noreply_address" mailNoreplyAddress cronMailNoreplyAddress *>
    checkEq "cgi_grp"              cgiGrpConfig       cronCgiGrpConfig       *>
    checkEq "mixpanel"             mixpanelToken      cronMixpanelToken      *>
    checkEq "salesforce"           salesforceConf     cronSalesforceConf
  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @CronConf)

checkFieldsEqualAppConfMailerConf :: AppConf -> MailingServerConf
                                  -> ConfigValidation
checkFieldsEqualAppConfMailerConf
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps           amazonConfig
     dbConfig           _maxDBConnections    redisCacheConfig  _localFileCacheSize
    _logConfig          _production         _cdnBaseUrl        _guardTimeConf
    _isMailBackdoorOpen _mailNoreplyAddress _cgiGrpConfig      _admins
    _sales              _initialUsers       _mixpanelToken     _trackjsToken
    _hubspotConf        _salesforceConf     _netsConfig        _monitoringConfig)
  (MailingServerConf
    _mailerHttpBindAddress     mailerDBConfig
    _mailerMaxDBConnections    mailerRedisCacheConfig
    _mailerLocalFileCacheSize _mailerLogConfig
    _mailerMasterSender       _mailerSlaveSender
     mailerAmazonConfig       _mailerTestReceivers
    _mailerMonitoringConfig )

  = checkEq "amazon"      amazonConfig     mailerAmazonConfig     *>
    checkEq "database"    dbConfig         mailerDBConfig         *>
    checkEq "redis_cache" redisCacheConfig mailerRedisCacheConfig
  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @MailingServerConf)

checkFieldsEqualAppConfMessengerConf :: AppConf -> MessengerServerConf
                                     -> ConfigValidation
checkFieldsEqualAppConfMessengerConf
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps          _amazonConfig
     dbConfig           _maxDBConnections   _redisCacheConfig  _localFileCacheSize
    _logConfig          _production         _cdnBaseUrl        _guardTimeConf
    _isMailBackdoorOpen _mailNoreplyAddress _cgiGrpConfig      _admins
    _sales              _initialUsers       _mixpanelToken     _trackjsToken
    _hubspotConf        _salesforceConf     _netsConfig        _monitoringConfig)
  (MessengerServerConf
    _messengerHttpBindAddress   messengerDBConfig
    _messengerMaxDBConnections _messengerLogConfig
    _messengerSenderDefault    _messengerSenderTelia
    _messengerMonitoringConfig)

  = checkEq "database"  dbConfig messengerDBConfig
  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @MessengerServerConf)

generateDefaultConfigFiles :: IO ()
generateDefaultConfigFiles = do
  writeDefaultConfigFile @AppConf
  writeDefaultConfigFile @CronConf
  writeDefaultConfigFile @MailingServerConf
  writeDefaultConfigFile @MessengerServerConf

writeDefaultConfigFile ::
  forall conf . (ConfigFile conf, Unjson conf, Default conf) => IO ()
writeDefaultConfigFile =
  writeDefaultConfig @conf putStrLn (configFile @conf) Proxy

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  if | args == [] || head args `elem` ["help"] -> printHelpMessage prog
     | head args `elem` ["lint", "check"]      -> lintConfigFiles
     | head args `elem` ["def", "default", "gen", "generate"]
                                               -> generateDefaultConfigFiles
     | otherwise                               -> printHelpMessage prog
