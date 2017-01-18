{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shake.Oracles where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Maybe
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
newtype NginxConfPath = NginxConfPath ()
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

addOracles :: Rules ()
addOracles = do
  -- * Oracles for using environment variables.
  -- See Shake documentation or newtype declarations for some background.
  _ <- addOracle $ \(GhcVersion _) ->
                     withVerbosity Silent $
                     (fromStdout <$>
                      cmd "ghc --numeric-version" :: Action String)
  _ <- addOracle $ \(TeamCity _) ->
                     not . null . fromMaybe ""
                     <$> getEnv "TEAMCITY_VERSION"
  -- This is needed by our build.
  -- FIXME should be part of SHAKE_BUILD_ env vars?
  _ <- addOracle $ \(NginxConfPath _) ->
                     fromMaybe "" <$> getEnv "NGINX_CONF_PATH"
  -- These are our build options
  _ <- addOracle $ \(BuildTarget _)        ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_TARGET"
  _ <- addOracle $ \(BuildSandbox _)       ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_SANDBOX"
  _ <- addOracle $ \(BuildTestConfPath _)  ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_TEST_CONF_PATH"
  _ <- addOracle $ \(BuildDev _)     ->
                     not . null . fromMaybe ""
                     <$> getEnv "SHAKE_BUILD_DEV"
  _ <- addOracle $ \(BuildTestCoverage _) ->
                     not . null . fromMaybe ""
                     <$> getEnv "SHAKE_BUILD_TEST_COVERAGE"
  _ <- addOracle $ \(BuildCabalConfigureOptions _)  ->
                     fromMaybe "" <$> getEnv "SHAKE_BUILD_CABAL_CONFIGURE_OPTS"
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

    nginxconfpath <- askOracle (NginxConfPath ())
    explainVar "NGINX_CONF_PATH" "Used for generating NGINX urls.txt file"
    showVarVal nginxconfpath

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
