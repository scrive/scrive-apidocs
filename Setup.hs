#!/usr/bin/env runghc
{-# OPTIONS_GHC -Wall #-}

module Main where

{-

This script serves the following purposes:

1. Creates version info from TeamCity delivered variables, like so:

    module Paths_kontrakcja (
        version,
        ) where

    version :: Version
    version = Version {versionBranch = [1,0,0,9999,2012,1,9,19,39,9],
                       versionTags = ["9999","2012-01-09 19:39:09",
                                      "e36529db1313333b7eb010c775854efca7d3b793"]}

  where versionBranch is:
   - 3 numbers taken from .cabal
   - BUILD_NUMBER from TeamCity
   - year, month, day, hour, min, sec taken as current utc time
  and versionTags is:
   - BUILD_NUMBER as string
   - formated date (utc again)
   - git commit id

2. Creates symlinks to build directories, so it builds faster. For
  example dist/build/dbdump/dbdump-tmp will be linked to
  dist/build/kontrakcja-server/kontrakcja-server-tmp.

3. Connects trhsx filer program.

-}

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Environment (getEnv)
import System.Time
import System.Time.ParseDate
import System.Locale
import qualified Control.Exception as E

trhsxProgram :: Program
trhsxProgram = simpleProgram "trhsx"

scriveConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
scriveConfHook a b = do
  localBuildInfo' <- confHook simpleUserHooks a b

  let localPkgDescr' = localPkgDescr localBuildInfo'
      package' = package localPkgDescr'
      pkgVersion' = pkgVersion package'

  now <- getClockTime
  let utc' = toUTCTime now

  let dateStr = formatCalendarTime defaultTimeLocale "%Y-%m-%d-%H%-%M-%S" utc'

  let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
      catchIO = E.catch
  buildNumber <- catchIO (getEnv "BUILD_NUMBER" >>= readIO) (const (return 0))
  buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER") (const (return ""))
  buildDate <- catchIO (getEnv "BUILD_DATE") (const (return dateStr))

  let Just utc = parseCalendarTime defaultTimeLocale "%Y-%m-%d-%H%-%M-%S" buildDate

  let pkgVersion'' = pkgVersion' { versionBranch =
                                     take 3 (versionBranch pkgVersion' ++ [0,0,0]) ++
                                     [ buildNumber
                                     , ctYear utc, fromEnum (ctMonth utc) + 1, ctDay utc
                                     , ctHour utc, ctMin utc, ctSec utc ]
                                 , versionTags = [buildDate, show buildNumber, buildVcsNumber]
                                 }
      package'' = package' { pkgVersion = pkgVersion'' }
      localPkgDescr'' = localPkgDescr' { package = package'' }
      localBuildInfo'' = localBuildInfo' { localPkgDescr = localPkgDescr'' }

  return localBuildInfo''

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
       { hookedPrograms = [trhsxProgram]
       , confHook = scriveConfHook
       }
