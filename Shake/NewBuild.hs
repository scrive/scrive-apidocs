-- | Various new-build related utilities.
--

module Shake.NewBuild (UseNewBuild(..)
                      ,useNewBuild
                      ,mkUseNewBuild
                      ,ifNewBuild
                      ,componentTargetPath
                      ,componentBuildRules) where

import Control.Monad
import Extra
import Development.Shake
import Development.Shake.FilePath
import System.Info (os, arch)
import System.Process (readProcess)
import qualified Data.Map as M

import Shake.Flags
import Shake.GetCabalDeps
import Shake.Utils

-- |
data UseNewBuild = UseNewBuild FilePath -- ^ New-build build dir.
                 | DontUseNewBuild

-- | Is 'new-build' mode enabled?
useNewBuild :: UseNewBuild -> Bool
useNewBuild (UseNewBuild _) = True
useNewBuild DontUseNewBuild = False

-- | Make a 'UseNewBuild' object from command-line flags.
mkUseNewBuild :: [ShakeFlag] -> IO UseNewBuild
mkUseNewBuild flags = if NewBuild `elem` flags
  then do ghcVer <- trim <$> readProcess "ghc" ["--numeric-version"] ""
          return . UseNewBuild $ "dist-newstyle" </> "build"
            </> (arch ++ "-" ++ os)
            </> ("ghc-" ++ ghcVer) </> "kontrakcja-1.0"
  else return DontUseNewBuild

-- | Branch based on whether new-build is enabled.
ifNewBuild :: UseNewBuild -> (FilePath -> m a) -> m a -> m a
ifNewBuild (UseNewBuild buildDir) act0 _act1 = act0 buildDir
ifNewBuild DontUseNewBuild       _act0  act1 = act1


-- | Given a component name, return the path to the corresponding
-- executable.
componentTargetPath :: UseNewBuild -> String -> FilePath
componentTargetPath DontUseNewBuild componentName =
  "dist" </> "build" </> componentName </> componentName <.> exe
componentTargetPath (UseNewBuild buildDir) componentName =
  buildDir </> "c" </> componentName </> "build"
           </> componentName </> componentName <.> exe

-- | For each exe/test-suite/benchmark component in the .cabal file,
-- add a rule for building the corresponding executable.
componentBuildRules :: UseNewBuild -> HsSourceDirsMap -> Rules ()
componentBuildRules newBuild hsSourceDirs = do
  forM_ (M.keys hsSourceDirs) $ \componentName ->
    if componentName /= ""
    then (componentTargetPath newBuild componentName) %> \_ ->
      -- Assumes that all sources of a component are in its hs-source-dirs.
      do let sourceDirs = componentHsSourceDirs componentName hsSourceDirs
         need ["dist/setup-config"]
         needPatternsInDirectories ["//*.hs"] sourceDirs
         if useNewBuild newBuild
           then cmd $ "cabal new-build " ++ componentName
           else cmd $ "cabal build " ++ componentName
    else return ()
