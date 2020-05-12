{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shake.Cabal ( CabalFile(packageId, allExtensions)
                   , parseCabalFile
                   , CabalComponentName, unComponentName
                   , mkExeName, mkSubLibName, mkTestName, mkBenchName
                   , Library, Executable, TestSuite, Benchmark, ForeignLib
                   , componentNameHasType
                   , allComponentNames
                   , componentDependencies
                   , libExeComponentNames
                   , testComponentNames, benchComponentNames
                   , allHsSourceDirs, allHsSourceDirsForComponentType
                   , componentHsSourceDirs
                   , allLibExeHsSourceDirs
                   , allTestHsSourceDirs
                   , allBenchHsSourceDirs
                   , allFLibHsSourceDirs
                   ) where

import Data.Bifunctor
import Data.Maybe
import Distribution.Backpack.ComponentsGraph
import Distribution.Compat.Graph as Graph
import Distribution.Compat.Lens as Lens
import Distribution.PackageDescription hiding (allExtensions)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Text (display)
import Distribution.Types.BuildInfo.Lens as Lens
import Distribution.Types.Component
import Distribution.Types.ComponentName
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity
import Language.Haskell.Extension
import qualified Data.Map.Strict as M
import qualified Distribution.PackageDescription as PkgDesc
import qualified Text.PrettyPrint as PP

import Shake.Utils

type CabalComponentName = ComponentName

unComponentName :: CabalComponentName -> String
unComponentName = maybe "" unUnqualComponentName . componentNameString

mkExeName, mkSubLibName, mkTestName, mkBenchName :: String -> ComponentName
mkExeName = CExeName . mkUnqualComponentName
mkSubLibName = CLibName . LSubLibName . mkUnqualComponentName
mkTestName = CTestName . mkUnqualComponentName
mkBenchName = CBenchName . mkUnqualComponentName

class ComponentNameHasType t where
  componentNameHasType :: ComponentName -> Bool

instance ComponentNameHasType Library where
  componentNameHasType (CLibName _) = True
  componentNameHasType _            = False
instance ComponentNameHasType Executable where
  componentNameHasType (CExeName _) = True
  componentNameHasType _            = False
instance ComponentNameHasType TestSuite where
  componentNameHasType (CTestName _) = True
  componentNameHasType _             = False
instance ComponentNameHasType Benchmark where
  componentNameHasType (CBenchName _) = True
  componentNameHasType _              = False
instance ComponentNameHasType ForeignLib where
  componentNameHasType (CFLibName _) = True
  componentNameHasType _             = False

-- | A component name -> list of hs-source-dirs map.
type HsSourceDirsMap = M.Map ComponentName [FilePath]

type ComponentDepsMap = M.Map ComponentName [ComponentName]

-- | All data that we need from a .cabal file.
data CabalFile = CabalFile {
  -- | Package identifier of this package (e.g. 'foo-2.1').
  packageId          :: String,
  -- | List all extensions used by this package.
  allExtensions      :: [Extension],
  -- | hs-source-dirs of all components. Default library has name
  -- "".
  hsSourceDirsMap    :: HsSourceDirsMap,
  -- | Component dependency graph.
  componentDepsMap   :: ComponentDepsMap
  }

cabalComponentHsSourceDirs :: Component -> [FilePath]
cabalComponentHsSourceDirs = Lens.view (Lens.buildInfo . Lens.hsSourceDirs)

-- | Parse a .cabal file.
parseCabalFile :: FilePath -> IO CabalFile
parseCabalFile cabalFile = do
  pkgDesc <- flattenPackageDescription <$> readGenericPackageDescription normal cabalFile

  let pkgid = display . package $ pkgDesc
      exts  = ordNub . concatMap PkgDesc.allExtensions . allBuildInfo $ pkgDesc
      compReqSpec =
        ComponentRequestedSpec { testsRequested = True, benchmarksRequested = True }
      compGraphE = mkComponentsGraph compReqSpec pkgDesc
      srcDirs    = ordNub . cabalComponentHsSourceDirs

  compGraph <- case compGraphE of
    Left  compCycle -> fail . PP.renderStyle PP.style . componentCycleMsg $ compCycle
    Right g         -> return g

  return $ CabalFile
    { packageId        = pkgid
    , allExtensions    = exts
    , hsSourceDirsMap  = M.map (srcDirs . Graph.nodeValue) . Graph.toMap $ compGraph
    , componentDepsMap = M.fromList
                         . map (first componentName)
                         . componentsGraphToList
                         $ compGraph
    }

-- | All components that this component depends on.
componentDependencies :: ComponentName -> CabalFile -> [ComponentName]
componentDependencies cname = M.findWithDefault [] cname . componentDepsMap

-- | List of hs-source-dirs of all components.
allHsSourceDirs :: CabalFile -> [FilePath]
allHsSourceDirs = ordNub . concat . M.elems . hsSourceDirsMap

-- | List of hs-source-dirs of all components of a given type.
allHsSourceDirsForComponentType
  :: forall  t . ComponentNameHasType t => CabalFile -> [FilePath]
allHsSourceDirsForComponentType cabalFile = allHsSourceDirs $ cabalFile
  { hsSourceDirsMap = M.filterWithKey (\k _v -> componentNameHasType @t k)
                        $ hsSourceDirsMap cabalFile
  }

-- | List of hs-source-dirs of a single component.
componentHsSourceDirs :: CabalFile -> ComponentName -> [FilePath]
componentHsSourceDirs cabalFile compName =
  fromMaybe [] . M.lookup compName . hsSourceDirsMap $ cabalFile

-- | Return the list of hs-source-dirs for all libraries and executables.
allLibExeHsSourceDirs :: CabalFile -> [FilePath]
allLibExeHsSourceDirs cabalFile = allHsSourceDirsForComponentType @Library cabalFile
  ++ allHsSourceDirsForComponentType @Executable cabalFile

-- | Return the list of hs-source-dirs for all test suites.
allTestHsSourceDirs :: CabalFile -> [FilePath]
allTestHsSourceDirs = allHsSourceDirsForComponentType @TestSuite

-- | Return the list of hs-source-dirs for all benchmarks.
allBenchHsSourceDirs :: CabalFile -> [FilePath]
allBenchHsSourceDirs = allHsSourceDirsForComponentType @Benchmark

-- | Return the list of hs-source-dirs for all foreign libraries.
allFLibHsSourceDirs :: CabalFile -> [FilePath]
allFLibHsSourceDirs = allHsSourceDirsForComponentType @ForeignLib

-- | List the names of all components.
allComponentNames :: CabalFile -> [ComponentName]
allComponentNames = M.keys . hsSourceDirsMap

-- | List all names of library/executable components.
libExeComponentNames :: CabalFile -> [ComponentName]
libExeComponentNames = filter isLibOrExe . allComponentNames
  where
    isLibOrExe n = componentNameHasType @Library n || componentNameHasType @Executable n

-- | List all names of test suite components.
testComponentNames :: CabalFile -> [ComponentName]
testComponentNames = filter (componentNameHasType @TestSuite) . allComponentNames

-- | List all names of benchmark components.
benchComponentNames :: CabalFile -> [ComponentName]
benchComponentNames = filter (componentNameHasType @Benchmark) . allComponentNames
