-- | Various new-build related utilities.
--

module Shake.NewBuild (UseNewBuild(DontUseNewBuild)
                      ,useNewBuild
                      ,mkUseNewBuild
                      ,ifNewBuild
                      ,whenNewBuild
                      ,componentTargetPath
                      ,componentBuildRules) where

import Control.Monad
import Data.Maybe
import Data.List.Extra (trim)
import Data.Version
import Data.Version.Extra (readVersion)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Text
import Distribution.System
import System.Process (readProcess)
import qualified Data.Map as M

import Shake.Cabal
import Shake.Flags
import Shake.Utils

newtype CabalInstallVersion = CabalInstallVersion Version

-- | cabal-install >= 2.1 uses different directories for different
-- types of components instead of putting everything under '/c'.
useSeparateComponentDirs :: CabalInstallVersion -> Bool
useSeparateComponentDirs (CabalInstallVersion v) = v > makeVersion [2,1]

type BuildDirPath = FilePath

-- | Whether new-build mode should be used, plus some settings data.
data UseNewBuild = UseNewBuild CabalInstallVersion BuildDirPath
                 | DontUseNewBuild

-- | Is 'new-build' mode enabled?
useNewBuild :: UseNewBuild -> Bool
useNewBuild (UseNewBuild _ _) = True
useNewBuild DontUseNewBuild   = False

-- | Make a 'UseNewBuild' object from command-line flags.
mkUseNewBuild :: [ShakeFlag] -> CabalFile -> IO UseNewBuild
mkUseNewBuild flags cabalFile =
  if NewBuild `elem` flags
  then do
    cabalVer <- readVersion <$> numericVersion "cabal"
    if cabalVer < makeVersion [1,25] then do
      putStrLn $ "Warning: --new-build only works with cabal-install >= 1.25."
      putStrLn "Falling back to old code path."
      return DontUseNewBuild
    else UseNewBuild <$> (pure . CabalInstallVersion $ cabalVer)
                     <*> (newBuildBuildDir <$> readGhcInfo)
  else return DontUseNewBuild

  where
    numericVersion   prog    = trim <$>
                               readProcess prog ["--numeric-version"] ""
    readGhcInfo              = M.fromList . read . trim
                               <$> readProcess "ghc" ["--info"] ""
    lookupGhcVersion ghcInfo = M.findWithDefault "???" "Project version" ghcInfo
    lookupGhcTarget  ghcInfo = fromMaybe "???" . join .
                               fmap (fmap display . platformFromTriple) $
                               M.lookup "Target platform" ghcInfo
    newBuildBuildDir ghcInfo = "dist-newstyle" </> "build"
                              </> (lookupGhcTarget ghcInfo)
                              </> ("ghc-" ++ lookupGhcVersion ghcInfo)
                              </> (packageId cabalFile)

-- | Branch based on whether new-build is enabled.
ifNewBuild :: UseNewBuild -> (FilePath -> a) -> a -> a
ifNewBuild (UseNewBuild _cabalVer buildDir) act0 _act1 = act0 buildDir
ifNewBuild DontUseNewBuild                  _act0 act1 = act1

-- | Side-effecting version of 'ifNewBuild'.
whenNewBuild :: Monad m => UseNewBuild -> (FilePath -> m ()) -> m ()
whenNewBuild unb act = ifNewBuild unb act (return ())

-- | Subdirectory where the built artifacts for this component are
-- located, this depends on the version of 'cabal-install'.
componentSubDir :: CabalInstallVersion -> CabalComponentName -> String
componentSubDir cabalInstallVer cname
  | useSeparateComponentDirs cabalInstallVer =
      case cname of
        LibraryName         _ -> "c"
        ExecutableName      _ -> "x"
        TestSuiteName       _ -> "t"
        BenchmarkName       _ -> "b"
        ForeignLibraryName  _ -> "f"
  | otherwise = "c"

-- | Given a component name and type, return the path to the
-- corresponding executable.
componentTargetPath :: UseNewBuild -> CabalComponentName -> FilePath
componentTargetPath DontUseNewBuild c =
  "dist" </> "build" </> componentName c </> componentName c <.> exe
componentTargetPath (UseNewBuild cabalInstallVer buildDir) c =
  buildDir </> componentSubDir cabalInstallVer c </> componentName c </> "build"
           </> componentName c </> componentName c <.> exe

-- | For each exe/test-suite/benchmark component in the .cabal file,
-- add a rule for building the corresponding executable.
componentBuildRules :: UseNewBuild -> CabalFile -> Rules ()
componentBuildRules newBuild cabalFile =
  forM_ (allComponentNames cabalFile) $ \cname ->
    if not . isLibrary $ cname
    then do
      let targetPath = componentTargetPath newBuild cname
      componentName cname ~> need [targetPath]
      targetPath %> \_ ->
        -- Assumes that all sources of a component are in its hs-source-dirs.
        do let sourceDirs = componentHsSourceDirs cname cabalFile
           if useNewBuild newBuild
             then need ["cabal.project.local"]
             else need ["dist/setup-config"]
           needPatternsInDirectories ["//*.hs"] sourceDirs
           if useNewBuild newBuild
             then cmd $ "cabal new-build " ++ (componentName cname)
             else cmd $ "cabal build " ++ (componentName cname)
    else return ()
