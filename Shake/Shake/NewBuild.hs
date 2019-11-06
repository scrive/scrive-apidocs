-- | Various v2-build related utilities.
--

module Shake.NewBuild (UseNewBuild(DontUseNewBuild)
                      ,useNewBuild
                      ,mkUseNewBuild
                      ,ifNewBuild
                      ,whenNewBuild
                      ,componentTargetPath
                      ,hpcPaths
                      ,componentBuildRules) where

import Control.Monad
import Data.List.Extra (trim)
import Data.Maybe
import Data.Version
import Data.Version.Extra (readVersion)
import Development.Shake
import Development.Shake.FilePath
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentName
import System.Process (readProcess)
import qualified Data.Map as M

import Shake.Cabal
import Shake.Flags
import Shake.Utils

newtype CabalInstallVersion = CabalInstallVersion Version

-- | cabal-install >= 2.1 uses different directories for different
-- types of components instead of putting everything under '/c'.
useSeparateComponentDirs :: CabalInstallVersion -> Bool
useSeparateComponentDirs (CabalInstallVersion v) = v > makeVersion [2, 1]

type BuildDirPath = FilePath

-- | Whether v2-build mode should be used, plus some settings data.
data UseNewBuild = UseNewBuild CabalInstallVersion BuildDirPath
                 | DontUseNewBuild

-- | Is 'v2-build' mode enabled?
useNewBuild :: UseNewBuild -> Bool
useNewBuild (UseNewBuild _ _) = True
useNewBuild DontUseNewBuild   = False

-- | Make a 'UseNewBuild' object from command-line flags.
mkUseNewBuild :: [ShakeFlag] -> CabalFile -> IO UseNewBuild
mkUseNewBuild flags cabalFile = if OldBuild `elem` flags
  then return DontUseNewBuild
  else do
    cabalVer <- readVersion <$> numericVersion "cabal"
    if cabalVer < makeVersion [1, 25]
      then do
        putStrLn $ "Warning: --v2-build only works with cabal-install >= 1.25."
        putStrLn "Falling back to old code path."
        return DontUseNewBuild
      else
        UseNewBuild
        <$> (pure . CabalInstallVersion $ cabalVer)
        <*> (newBuildBuildDir <$> readGhcInfo)
  where
    numericVersion prog = trim <$> readProcess prog ["--numeric-version"] ""
    readGhcInfo = M.fromList . read . trim <$> readProcess "ghc" ["--info"] ""
    lookupGhcVersion ghcInfo = M.findWithDefault "???" "Project version" ghcInfo
    lookupGhcTarget ghcInfo =
      fromMaybe "???" . join . fmap (fmap display . platformFromTriple) $ M.lookup
        "Target platform"
        ghcInfo
    newBuildBuildDir ghcInfo =
      "dist-newstyle"
        </> "build"
        </> (lookupGhcTarget ghcInfo)
        </> ("ghc-" <> lookupGhcVersion ghcInfo)
        </> (packageId cabalFile)

-- | Branch based on whether v2-build is enabled.
ifNewBuild :: UseNewBuild -> (FilePath -> a) -> a -> a
ifNewBuild (UseNewBuild _cabalVer buildDir) act0  _act1 = act0 buildDir
ifNewBuild DontUseNewBuild                  _act0 act1  = act1

-- | Side-effecting version of 'ifNewBuild'.
whenNewBuild :: Monad m => UseNewBuild -> (FilePath -> m ()) -> m ()
whenNewBuild unb act = ifNewBuild unb act (return ())

-- | Subdirectory where the built artifacts for this component are
-- located, this depends on the version of 'cabal-install'.
componentSubDir
  :: CabalInstallVersion -> OptimisationLevel -> CabalComponentName -> String
componentSubDir cabalInstallVer optlevel cname = ctype </> cbuilddir
  where
    ctype
      | useSeparateComponentDirs cabalInstallVer = case cname of
        CLibName      -> ""
        CSubLibName _ -> "l"
        CExeName    _ -> "x"
        CTestName   _ -> "t"
        CBenchName  _ -> "b"
        CFLibName   _ -> "f"
      | otherwise = "c"

    cbuilddir = case cname of
      CLibName -> unComponentName cname </> coptlevel </> "build"
      _ -> unComponentName cname </> coptlevel </> "build" </> unComponentName cname

    coptlevel = case optlevel of
      NoOptimisation      -> "noopt"
      DefaultOptimisation -> ""
      MaxOptimisation     -> "opt"

-- | Name of the main built artifact for this component.
componentArtifactName :: CabalComponentName -> String
componentArtifactName c =
  let libname = "libHS" <> unComponentName c <> "1.0-inplace" <.> "a"
  in  case c of
        CLibName      -> libname
        CSubLibName _ -> libname
        _             -> unComponentName c <.> exe

-- | Given a component name and type, return the path to the
-- corresponding executable.
componentTargetPath :: UseNewBuild -> OptimisationLevel -> ComponentName -> FilePath
componentTargetPath DontUseNewBuild _optlevel c =
  "dist" </> "build" </> unComponentName c </> componentArtifactName c
componentTargetPath (UseNewBuild cabalInstallVer buildDir) optlevel c =
  buildDir </> componentSubDir cabalInstallVer optlevel c </> componentArtifactName c


-- | Paths that coverage report generation needs to pass to `hpc` via
-- `--hpcdir`.
hpcPaths :: CabalFile -> UseNewBuild -> OptimisationLevel -> [FilePath]
hpcPaths cabalFile newBuild opt =
  [mainPath, componentPath . mkTestName $ "kontrakcja-test"] <> if useNewBuild newBuild
    then [componentPath . mkSubLibName $ "kontrakcja-prelude"]
    else []
  where
    build_dir = ifNewBuild newBuild id "dist"
    hdm       = "hpc" </> "dyn" </> "mix"
    mainPath  = build_dir </> hdm </> packageId cabalFile

    componentPath :: ComponentName -> FilePath
    componentPath c = case newBuild of
      DontUseNewBuild -> build_dir </> hdm </> unComponentName c
      (UseNewBuild cabalInstallVer _buildDir) ->
        build_dir
          </> componentSubDir cabalInstallVer opt c
          </> unComponentName c
          </> hdm
          </> (case c of
                CLibName -> packageId cabalFile
                _        -> unComponentName c
              )

-- | For each exe/test-suite/benchmark component in the .cabal file,
-- add a rule for building the corresponding executable.
componentBuildRules
  :: FilePath -> UseNewBuild -> OptimisationLevel -> CabalFile -> Rules ()
componentBuildRules sourceRoot newBuild optlevel cabalFile =
  forM_ (allComponentNames cabalFile) $ \cname -> do
    let targetPath = componentTargetPath newBuild optlevel cname
    unComponentName cname ~> do
      need [targetPath]
    targetPath %> \_ ->
      -- Assumes that all sources of a component are in its hs-source-dirs.
                        do
      let sourceDirs     = componentHsSourceDirs cabalFile cname
          depsSourceDirs = concatMap (componentHsSourceDirs cabalFile)
                                     (componentDependencies cname cabalFile)
      if useNewBuild newBuild
        then need ["cabal.project.local"]
        else need ["dist/setup-config"]
      needPatternsInDirectories sourceRoot
                                ["//*.hs"]
                                (ordNub $ sourceDirs <> depsSourceDirs)
      if useNewBuild newBuild
        then cmd $ "cabal v2-build " <> (unComponentName cname)
        else cmd $ "cabal build " <> (unComponentName cname)
