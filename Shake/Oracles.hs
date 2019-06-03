{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Shake.Oracles where

import Control.Monad
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Development.Shake
import Development.Shake.Classes

-- * These newtypes are used by `askOracle` to get environment variables
-- Treat them as black-boxes that you don't need to know about!
-- This is just an oddity of the way these "Oracles" work in Shake
-- The other option was to write the value of env vars to a file in the Shake
-- build directory, this option was preffered
newtype GhcVersion = GhcVersion ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCity = TeamCity ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype NginxConfDefaultRule = NginxConfDefaultRule ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype NginxConfRulesPath = NginxConfRulesPath ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype NginxConfRulesPathAlternative = NginxConfRulesPathAlternative ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTarget = BuildTarget ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildSandbox = BuildSandbox ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTestConfPath = BuildTestConfPath ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildDev = BuildDev ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTestCoverage = BuildTestCoverage ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildCabalConfigureOptions = BuildCabalConfigureOptions ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCityBuildDBConnString = TeamCityBuildDBConnString ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCityBuildDBName = TeamCityBuildDBName ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCityBuildLambdaConf = TeamCityBuildLambdaConf ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCityBuildS3Conf = TeamCityBuildS3Conf ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCityBuildCronMonthlyInvoice = TeamCityBuildCronMonthlyInvoice ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype CreateTestDBWithConfData = CreateTestDBWithConfData ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult GhcVersion                      = String
type instance RuleResult TeamCity                        = Bool
type instance RuleResult NginxConfRulesPath              = String
type instance RuleResult NginxConfDefaultRule            = String
type instance RuleResult NginxConfRulesPathAlternative   = String
type instance RuleResult BuildTarget                     = String
type instance RuleResult BuildSandbox                    = String
type instance RuleResult BuildTestConfPath               = String
type instance RuleResult BuildDev                        = Bool
type instance RuleResult BuildTestCoverage               = Bool
type instance RuleResult BuildCabalConfigureOptions      = String
type instance RuleResult TeamCityBuildDBConnString       = String
type instance RuleResult TeamCityBuildDBName             = String
type instance RuleResult TeamCityBuildLambdaConf         = String
type instance RuleResult TeamCityBuildS3Conf             = String
type instance RuleResult TeamCityBuildCronMonthlyInvoice = String
type instance RuleResult CreateTestDBWithConfData        = (String, String, String, String, String)

addOracles :: Rules ()
addOracles = do
  -- * Oracles for using environment variables.
  -- See Shake documentation or newtype declarations for some background.
  void $ addOracle $ \(GhcVersion _) ->
                     withVerbosity Silent $
                     (fromStdout <$>
                      cmd "ghc --numeric-version" :: Action String)
  void $ addOracle $ \(TeamCity _) ->
                     not . null . fromMaybe ""
                     <$> getEnv "TEAMCITY_VERSION"
  void $ addOracle $ \(TeamCityBuildDBConnString _)  ->
                     fromMaybe "" <$> getEnv "DB_BUILD_ADMIN_CONN_STRING"
  void $ addOracle $ \(TeamCityBuildDBName _)  ->
                     fromMaybe "" <$> getEnv "BUILD_DB_NAME"
  void $ addOracle $ \(TeamCityBuildLambdaConf _)  ->
                     fromMaybe "" <$> getEnv "TEST_LAMBDA_CONF"
  void $ addOracle $ \(TeamCityBuildS3Conf _)  ->
                     fromMaybe "" <$> getEnv "TEST_S3_CONF"
  void $ addOracle $ \(TeamCityBuildCronMonthlyInvoice _)  ->
                     fromMaybe "" <$> getEnv "TEST_CRON_MONTHLY_INVOICE"

  -- This is needed by our build.
  -- FIXME should be part of SHAKE_BUILD_ env vars?
  void $ addOracle $ \(NginxConfRulesPath _) ->
                     fromMaybe "" <$> getEnv "NGINX_CONF_RULES_PATH"
  void $ addOracle $ \(NginxConfDefaultRule _) ->
                     fromMaybe "" <$> getEnv "NGINX_CONF_DEFAULT_RULE"
  void $ addOracle $ \(NginxConfRulesPathAlternative _) ->
                     fromMaybe "" <$> getEnv "NGINX_CONF_RULES_PATH_ALTERNATIVE"
  -- These are our build options
  void $ addOracle $ \(BuildTarget _)        ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_TARGET"
  void $ addOracle $ \(BuildSandbox _)       ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_SANDBOX"
  void $ addOracle $ \(BuildTestConfPath _)  ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_TEST_CONF_PATH"
  void $ addOracle $ \(BuildDev _)     ->
                     not . null . fromMaybe ""
                     <$> getEnv "SHAKE_BUILD_DEV"
  void $ addOracle $ \(BuildTestCoverage _) ->
                     not . null . fromMaybe ""
                     <$> getEnv "SHAKE_BUILD_TEST_COVERAGE"
  void $ addOracle $ \(BuildCabalConfigureOptions _)  ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_CABAL_CONFIGURE_OPTS"
  void $ addOracle $ \(CreateTestDBWithConfData _) -> do
    tc  <- askOracle (TeamCity ())
    now <- liftIO $ getCurrentTime
    let defDBName     = formatTime defaultTimeLocale
                        "kontrakcja_test_%Y_%m_%d_%H_%M_%S"
                        now
        defConnString = "host='localhost' user='kontra' password='kontrapwd'"
    connString      <- if tc then askOracle (TeamCityBuildDBConnString ())
                             else return defConnString
    dbName          <- if tc then askOracle (TeamCityBuildDBName ())
                             else return defDBName
    lConf           <- if tc then askOracle (TeamCityBuildLambdaConf ())
                             else error
                             "AWS Lambda configuration for tests must be defined"
                             :: m String
    s3Conf          <- if tc then askOracle (TeamCityBuildS3Conf ())
                             else error
                             "AWS S3 configuration for tests must be defined"
                             :: m String
    cronInvoiceConf <- if tc then askOracle (TeamCityBuildCronMonthlyInvoice ())
                             else error
                             "Cron monthly-invoice configuration for tests must be defined"
                             :: m String
    return $ (dbName, connString, lConf, s3Conf, cronInvoiceConf)

  return ()

oracleHelpRule :: Rules ()
oracleHelpRule = do
  -- * Helpful printing of environment variables used
  "help-env" ~> do
    putNormal "# Environment variables (normally only used by CI builds):"
    putNormal ""

    target <- askOracle (BuildTarget ())
    explainVar "SHAKE_BUILD_TARGET" "master, staging, or production"
    showVarVal target

    sandbox <- askOracle (BuildSandbox ())
    explainVar "SHAKE_BUILD_SANDBOX" "Path to sandbox to use"
    showVarVal sandbox

    testconf <- askOracle (BuildTestConfPath ())
    explainVar "SHAKE_BUILD_TEST_CONF_PATH"
               "Path to kontrakcja_test.conf file to use"
    showVarVal testconf

    devBuild <- askOracleWith (BuildDev ()) True
    explainVar "SHAKE_BUILD_DEV" "If not empty, will not run clean bulid"
    showVarVal (show devBuild)

    testCoverage <- askOracleWith (BuildTestCoverage ()) True
    explainVar "SHAKE_BUILD_TEST_COVERAGE"
      "If not empty, will create a coverage report from server tests"
    showVarVal (show testCoverage)

    cabalFlags <- askOracleWith (BuildCabalConfigureOptions ()) ""
    explainVar "SHAKE_BUILD_CABAL_CONFIGURE_OPTS "
               "Custom flags to pass to 'cabal configure'"
    showVarVal cabalFlags

    putNormal ""

    tc <- askOracleWith (TeamCity ()) True
    explainVar "TEAMCITY_VERSION" "Used to determine if run by TeamCity CI"
    showVarVal (show tc)

    tcDBCS <- askOracleWith (TeamCityBuildDBConnString ()) ""
    explainVar "BUILD_DB_ADMIN_CONN_STRING" $
      "Postgres connection string for admin access to the"
      ++ " isolated per-build DB. Doesn't include the DB name."
    showVarVal (show tcDBCS)

    tcDB <- askOracleWith (TeamCityBuildDBName ()) ""
    explainVar "BUILD_DB_NAME" $ "Name of the isolated per-build DB."
      ++ " Normally consists of a prefix and a build number."
    showVarVal (show tcDB)

    nginxconfrulespath <- askOracle (NginxConfRulesPath ())
    explainVar "NGINX_CONF_RULES_PATH" "Used for generating NGINX urls.txt file"
    showVarVal nginxconfrulespath

    nginxconfdefaultrule <- askOracle (NginxConfDefaultRule ())
    explainVar "NGINX_CONF_DEFAULT_RULE" "Used for generating NGINX urls.txt file"
    showVarVal nginxconfdefaultrule

    nginxconfrulespathalternative <- askOracle (NginxConfRulesPathAlternative ())
    explainVar "NGINX_CONF_RULES_PATH_ALTERNATIVE" "Used for generating NGINX urls_list.txt file"
    showVarVal nginxconfrulespathalternative

    putNormal ""

    ghc <- askOracleWith (GhcVersion ()) ""
    explainVar "GHC version" ghc

    putNormal ""

  "env" ~> do
    need ["help-env"]
    cmd "env"

explainVar :: String -> String -> Action ()
explainVar var desc = putNormal $ indent0 ++ var ++ indent1 ++ ": " ++ desc
  where
    l = length var
    i = 27 - l
    indent0 = replicate 3 ' '
    indent1 = if l > 27 then "\n" ++ replicate 30 ' ' else replicate i ' '

showVarVal :: String -> Action ()
showVarVal val = putNormal $ (replicate 30 ' ') ++ "= " ++ val
