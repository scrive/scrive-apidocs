{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Data.Either.Validation
import Data.Unjson
import System.Environment
import System.Exit
import System.FilePath ((</>), FilePath)
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import AppConf
import AppDir
import Configuration
import CronConf
import MailingServerConf
import MessengerServerConf

printHelpMessage :: Text -> IO ()
printHelpMessage prog =
  TIO.putStrLn
    . T.unlines
    $ ["Usage: ", "", prog <> " check - Check that config files are in sync."]

-- | Map between parsed configuration types (like 'AppConf') and file names.
class ConfigFile config where
  -- | Given a parsed configuration type, return the config file name.
  configFile :: FilePath

readConfigFile
  :: forall  config . (ConfigFile config, Unjson config) => FilePath -> IO config
readConfigFile workspaceRoot =
  readConfig putStrLn $ workspaceRoot </> (configFile @config)

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
  (AppPaths _ workspaceRoot) <- setupAppPaths

  appConf                    <- readConfigFile @AppConf workspaceRoot
  cronConf                   <- readConfigFile @CronConf workspaceRoot
  mailerConf                 <- readConfigFile @MailingServerConf workspaceRoot
  messengerConf              <- readConfigFile @MessengerServerConf workspaceRoot

  putStrLn "Checking that config files are in sync..."

  case
      checkFieldsEqualAppConfCronConf appConf cronConf
      *> checkFieldsEqualAppConfMailerConf appConf mailerConf
      *> checkFieldsEqualAppConfMessengerConf appConf messengerConf
    of
      Success ()   -> putStrLn "Checks finished."
      Failure errs -> do
        forM_ errs $ hPutStrLn stderr
        exitFailure

type ConfigValidation = Validation [String] ()

checkFieldEq :: Eq a => (FilePath, FilePath) -> String -> a -> a -> ConfigValidation
checkFieldEq (conf0, conf1) fieldName a b = if a == b
  then Success ()
  else Failure
    [ "Field '"
      <> fieldName
      <> "' in '"
      <> conf0
      <> "' and '"
      <> conf1
      <> "' is not in sync."
    ]

-- brittany-disable-next-binding
checkFieldsEqualAppConfCronConf :: AppConf -> CronConf
                                -> ConfigValidation
checkFieldsEqualAppConfCronConf
   -- Verbose pattern matching here is deliberate, so that we get a
   -- warning when a new field is added.
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps           amazonConfig
     dbConfig           _maxDBConnections   _queryTimeout       redisCacheConfig
    _localFileCacheSize _logConfig          _readOnlyDatabase    _production
    _cdnBaseUrl
     guardTimeConf      _isMailBackdoorOpen  mailNoreplyAddress  cgiGrpConfig
    _admins             _sales              _initialUsers        mixpanelToken
    _gaToken            _trackjsToken       _zendeskKey         _hubspotConf
    salesforceConf      _netsConfig         _monitoringConfig   _isAPILogEnabled
    netsSignConfig      pdfToolsLambdaConf  _passwordServiceConf _eidServiceConf)
  (CronConf
     cronAmazonConfig              cronDBConfig                   _cronMaxDBConnections
     cronRedisCacheConfig         _cronLocalFileCacheSize         _cronLogConfig
     cronGuardTimeConf             cronCgiGrpConfig                cronMixpanelToken
    _cronNtpServers                cronSalesforceConf
    _cronPlanhatConf              _cronMonitoringConf              cronMailNoreplyAddress
    _cronConsumerCronMaxJobs      _cronConsumerSealingMaxJobs     _cronConsumerSigningMaxJobs
    _cronConsumerExtendingMaxJobs _cronConsumerAPICallbackMaxJobs _cronConsumerFilePurgingMaxJobs
     cronNetsSignConfig           cronPdfToolsLambdaConf          _cronMonthlyInvoiceConf
    _cronStatsDConf)

  = checkEq "amazon"               amazonConfig       cronAmazonConfig       *>
    checkEq "database"             dbConfig           cronDBConfig           *>
    checkEq "redis_cache"          redisCacheConfig   cronRedisCacheConfig   *>
    checkEq "guardtime"            guardTimeConf      cronGuardTimeConf      *>
    checkEq "mail_noreply_address" mailNoreplyAddress cronMailNoreplyAddress *>
    checkEq "cgi_grp"              cgiGrpConfig       cronCgiGrpConfig       *>
    checkEq "mixpanel"             mixpanelToken      cronMixpanelToken      *>
    checkEq "salesforce"           salesforceConf     cronSalesforceConf     *>
    checkEq "nets_sign"            netsSignConfig     cronNetsSignConfig     *>
    checkEq "pdftools_lambda"      pdfToolsLambdaConf cronPdfToolsLambdaConf

  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @CronConf)

-- brittany-disable-next-binding
checkFieldsEqualAppConfMailerConf :: AppConf -> MailingServerConf
                                  -> ConfigValidation
checkFieldsEqualAppConfMailerConf
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps           amazonConfig
     dbConfig           _maxDBConnections   _queryTimeout       redisCacheConfig
    _localFileCacheSize _logConfig          _readOnlyDatabase   _production
    _cdnBaseUrl         _guardTimeConf      _isMailBackdoorOpen _mailNoreplyAddress
    _cgiGrpConfig       _admins             _sales              _initialUsers
    _mixpanelToken      _gaToken            _trackjsToken       _zendeskKey
    _hubspotConf        _salesforceConf     _netsConfig         _monitoringConfig
    _isAPILogEnabled    _netsSignConfig     _pdfToolsLambdaConf _passwordServiceConf
    _eidServiceConf)
  (MailingServerConf
    _mailerHttpBindAddress     mailerDBConfig
    _mailerMaxDBConnections    mailerRedisCacheConfig
    _mailerLocalFileCacheSize _mailerLogConfig
    _mailerMasterSender       _mailerSlaveSender
     mailerAmazonConfig       _mailerTestReceivers
    _mailerMonitoringConfig)

  = checkEq "amazon"      amazonConfig     mailerAmazonConfig     *>
    checkEq "database"    dbConfig         mailerDBConfig         *>
    checkEq "redis_cache" redisCacheConfig mailerRedisCacheConfig
  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @MailingServerConf)

-- brittany-disable-next-binding
checkFieldsEqualAppConfMessengerConf :: AppConf -> MessengerServerConf
                                     -> ConfigValidation
checkFieldsEqualAppConfMessengerConf
  (AppConf
    _httpBindAddress    _mainDomainUrl      _useHttps           _amazonConfig
     dbConfig           _maxDBConnections   _queryTimeout       _redisCacheConfig
    _localFileCacheSize _logConfig          _readOnlyDatabase   _production
    _cdnBaseUrl         _guardTimeConf      _isMailBackdoorOpen _mailNoreplyAddress
    _cgiGrpConfig       _admins             _sales              _initialUsers
    _mixpanelToken      _gaToken            _trackjsToken       _zendeskKey
    _hubspotConf        _salesforceConf     _netsConfig         _monitoringConfig
    _isAPILogEnabled    _netsSignConfig     _pdfToolsLambdaConf _passwordServiceConf
    _eidServiceConf)
  (MessengerServerConf
    _messengerHttpBindAddress   messengerDBConfig
    _messengerMaxDBConnections _messengerLogConfig
    _messengerSenderDefault    _messengerSenderTelia
    _messengerMonitoringConfig)

  = checkEq "database"  dbConfig messengerDBConfig
  where
    checkEq :: forall a . Eq a => String -> a -> a -> ConfigValidation
    checkEq = checkFieldEq (configFile @AppConf, configFile @MessengerServerConf)

main :: IO ()
main = do
  prog <- T.pack <$> getProgName
  args <- fmap T.pack <$> getArgs
  if
    | args == [] || head args `elem` ["help"] -> printHelpMessage prog
    | head args `elem` ["lint", "check"] -> lintConfigFiles
    | otherwise -> printHelpMessage prog
