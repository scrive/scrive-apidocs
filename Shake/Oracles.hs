{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shake.Oracles where

import Control.Applicative
import Data.Maybe
import Development.Shake
import Development.Shake.Classes

-- * These newtypes are used by `askOracle` to get environment variables
-- Treat them as black-boxes that you don't need to know about!
-- This is just an oddity of the way these "Oracles" work in Shake
-- The other option was to write the value of env vars to a file in the Shake
-- build directory, this option was preffered
newtype GhcVersion = GhcVersion () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype TeamCity = TeamCity () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype NginxConfPath = NginxConfPath () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTarget = BuildTarget () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildSandbox = BuildSandbox () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTestConfPath = BuildTestConfPath () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildGitHub = BuildGitHub () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildDev = BuildDev () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildTestCoverage = BuildTestCoverage () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype BuildCabalConfigureOptions = BuildCabalConfigureOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

addOracles :: Rules ()
addOracles = do
  -- * Oracles for using environment variables
  -- See Shake documentation or newtype declarations for some background
  _ <- addOracle $ \(GhcVersion _) -> fmap fromStdout $ cmd "ghc --numeric-version" :: Action String
  _ <- addOracle $ \(TeamCity _) -> not . null . fromMaybe "" <$> getEnv "TEAMCITY_VERSION"
  -- This is needed by our build
  -- FIXME should be part of SHAKE_BUILD_ env vars?
  _ <- addOracle $ \(NginxConfPath _) -> fromMaybe "" <$> getEnv "NGINX_CONF_PATH"
  -- These are our build options
  _ <- addOracle $ \(BuildTarget _)        -> fromMaybe "" <$> getEnv "SHAKE_BUILD_TARGET"
  _ <- addOracle $ \(BuildSandbox _)       -> fromMaybe "" <$> getEnv "SHAKE_BUILD_SANDBOX"
  _ <- addOracle $ \(BuildTestConfPath _)  -> fromMaybe "" <$> getEnv "SHAKE_BUILD_TEST_CONF_PATH"
  _ <- addOracle $ \(BuildGitHub _)  ->       not . null . fromMaybe "" <$> getEnv "SHAKE_BUILD_GITHUB"
  _ <- addOracle $ \(BuildDev _)     ->       not . null . fromMaybe "" <$> getEnv "SHAKE_BUILD_DEV"
  _ <- addOracle $ \(BuildTestCoverage _) ->  not . null . fromMaybe "" <$> getEnv "SHAKE_BUILD_TEST_COVERAGE"
  _ <- addOracle $ \(BuildCabalConfigureOptions _)  -> fromMaybe "" <$> getEnv "SHAKE_BUILD_CABAL_CONFIGURE_OPTS"
  return ()

oracleHelpRule :: Rules ()
oracleHelpRule = do
  -- * Helpful printing of environment variables used
  "help-env" ~> do
    putNormal "# Environment variables (normally only used by CI builds):"
    putNormal ""
    putNormal   "   SHAKE_BUILD_TARGET         : master, staging, or production"
    target <- askOracle (BuildTarget ())
    putNormal $ "                              = " ++ target
    putNormal   "   SHAKE_BUILD_SANDBOX        : Path to sandbox to use"
    sandbox <- askOracle (BuildSandbox ())
    putNormal $ "                              = " ++ sandbox
    putNormal   "   SHAKE_BUILD_TEST_CONF_PATH : Path to kontrakcja_test.conf file to use"
    testconf <- askOracle (BuildTestConfPath ())
    putNormal $ "                              = " ++ testconf
    putNormal   "   SHAKE_BUILD_GITHUB         : If not empty, will ping GitHub with build status"
    gh <- askOracleWith (BuildGitHub ()) True
    putNormal $ "                              = " ++ show gh
    putNormal   "   SHAKE_BUILD_DEV            : If not empty, will not run clean bulid"
    devBuild <- askOracleWith (BuildDev ()) True
    putNormal $ "                              = " ++ show devBuild
    putNormal   "   SHAKE_BUILD_TEST_COVERAGE  : If not empty, will create a coverage report from server tests"
    testCoverage <- askOracleWith (BuildTestCoverage ()) True
    putNormal $ "                              = " ++ show testCoverage
    putNormal   "   SHAKE_BUILD_CABAL_CONFIGURE_OPTS : Custom flags to pass to cabal configure"
    cabalFlags <- askOracleWith (BuildCabalConfigureOptions ()) ""
    putNormal $ "                              = " ++ show cabalFlags
    putNormal ""
    putNormal   "   TEAMCITY_VERSION           : Used to determine if run by TeamCity CI"
    tc <- askOracleWith (TeamCity ()) True
    putNormal $ "                              = " ++ show tc
    nginxconfpath <- askOracle (NginxConfPath ())
    putNormal   "   NGINX_CONF_PATH            : Used for generating NGINX urls.txt file"
    putNormal $ "                              = " ++ nginxconfpath
    putNormal ""
    ghc <- askOracleWith (GhcVersion ()) ""
    putNormal $ "   GHC version: " ++ ghc
    putNormal ""

  "env" ~> do
    need ["help-env"]
    cmd "env"
