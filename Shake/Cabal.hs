{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif

module Shake.Cabal ( CabalFile(packageId, allExtensions), parseCabalFile
                   , CabalComponentName(..), componentName
                   , isLibrary, isExecutable, isLibOrExe
                   , isTestSuite, isBenchmark, isForeignLibrary
                   , CabalComponentType(..), componentNameHasType
                   , allComponentNames
                   , libExeComponentNames, testComponentNames, benchComponentNames
                   , allHsSourceDirs, allHsSourceDirsForComponentType
                   , componentHsSourceDirs
                   , allLibExeHsSourceDirs
                   , allTestHsSourceDirs
                   , allBenchHsSourceDirs
                   , allFLibHsSourceDirs
                   ) where

import qualified Data.Map.Strict                               as M
import           Data.Maybe
import           Distribution.PackageDescription               hiding
                                                               (allExtensions)
import qualified Distribution.PackageDescription               as PkgDesc
import           Distribution.PackageDescription.Configuration
#if MIN_VERSION_Cabal(2,2,0)
import           Distribution.PackageDescription.Parsec
#else
import           Distribution.PackageDescription.Parse
#endif
#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Types.UnqualComponentName
#endif
import           Distribution.Text                             (display)
import           Distribution.Verbosity
import           Language.Haskell.Extension

import Shake.Utils

#if !MIN_VERSION_Cabal(2,0,0)

unUnqualComponentName :: String -> String
unUnqualComponentName = id

readGenericPackageDescription :: Verbosity -> FilePath
                              -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif

data CabalComponentName = LibraryName        String
                        | ExecutableName     String
                        | TestSuiteName      String
                        | BenchmarkName      String
                        | ForeignLibraryName String
  deriving (Ord, Eq)

componentName :: CabalComponentName -> String
componentName (LibraryName n)        = n
componentName (ExecutableName n)     = n
componentName (TestSuiteName n)      = n
componentName (BenchmarkName n)      = n
componentName (ForeignLibraryName n) = n

isLibrary :: CabalComponentName -> Bool
isLibrary (LibraryName _) = True
isLibrary _               = False

isExecutable :: CabalComponentName -> Bool
isExecutable (ExecutableName _) = True
isExecutable _                  = False

isLibOrExe :: CabalComponentName -> Bool
isLibOrExe (LibraryName _)    = True
isLibOrExe (ExecutableName _) = True
isLibOrExe _                  = False

isTestSuite :: CabalComponentName -> Bool
isTestSuite (TestSuiteName _) = True
isTestSuite _                 = False

isBenchmark :: CabalComponentName -> Bool
isBenchmark (BenchmarkName _) = True
isBenchmark _                 = False

isForeignLibrary :: CabalComponentName -> Bool
isForeignLibrary (ForeignLibraryName _) = True
isForeignLibrary _                 = False

data CabalComponentType = LibraryComponent
                        | ExecutableComponent
                        | TestSuiteComponent
                        | BenchmarkComponent
                        | ForeignLibraryComponent

componentNameHasType :: CabalComponentType -> CabalComponentName -> Bool
componentNameHasType LibraryComponent        = isLibrary
componentNameHasType ExecutableComponent     = isExecutable
componentNameHasType TestSuiteComponent      = isTestSuite
componentNameHasType BenchmarkComponent      = isBenchmark
componentNameHasType ForeignLibraryComponent = isForeignLibrary


-- | A component name -> list of hs-source-dirs map.
type HsSourceDirsMap = M.Map CabalComponentName [FilePath]

-- | All data that we need from a .cabal file.
data CabalFile = CabalFile {
  -- | Package identifier of this package (e.g. 'foo-2.1').
  packageId          :: String,
  -- | List all extensions used by this package.
  allExtensions      :: [Extension],
  -- | hs-source-dirs of all components. Default library has name
  -- "". TODO: support sublibraries and foreign libraries.
  hsSourceDirsMap    :: HsSourceDirsMap
  }

-- | Parse a .cabal file.
parseCabalFile :: FilePath -> IO CabalFile
parseCabalFile cabalFile = do
  pkgDesc <- flattenPackageDescription <$>
             readGenericPackageDescription normal cabalFile
  let libBuildInfos    =  [(LibraryName "", libBuildInfo $ lib)
                          | lib <- maybeToList $ library pkgDesc ]
      exeBuildInfos    =  [( ExecutableName . unUnqualComponentName . exeName $ exe
                           , buildInfo exe )
                          | exe <- executables pkgDesc ]
      testBuildInfos   =  [( TestSuiteName . unUnqualComponentName . testName
                             $ test
                           , testBuildInfo test )
                          | test <- testSuites pkgDesc]
      benchBuildInfos  =  [( BenchmarkName . unUnqualComponentName . benchmarkName
                             $ bench
                           , benchmarkBuildInfo bench )
                          | bench <- benchmarks pkgDesc]
      srcDirs bis      =  M.fromList [(name, ordNub . hsSourceDirs $ bi)
                                     | (name, bi) <- bis]
      exts             = ordNub . concatMap PkgDesc.allExtensions . allBuildInfo $
                         pkgDesc
      pkgid            = display . package $ pkgDesc
  return $ CabalFile pkgid exts
                     (mconcat . map srcDirs $ [ libBuildInfos, exeBuildInfos
                                              , testBuildInfos, benchBuildInfos])

-- | List of hs-source-dirs of all components.
allHsSourceDirs :: CabalFile -> [FilePath]
allHsSourceDirs = ordNub . concat . M.elems . hsSourceDirsMap

-- | List of hs-source-dirs of all components of a given type.
allHsSourceDirsForComponentType :: CabalComponentType -> CabalFile -> [FilePath]
allHsSourceDirsForComponentType componentType cabalFile =
  allHsSourceDirs $ cabalFile
  { hsSourceDirsMap = M.filterWithKey
                      (\k _v -> componentNameHasType componentType k)
                      $ hsSourceDirsMap cabalFile
  }

-- | List of hs-source-dirs of a single component.
componentHsSourceDirs :: CabalComponentName -> CabalFile -> [FilePath]
componentHsSourceDirs compName = fromMaybe [] . M.lookup compName . hsSourceDirsMap

-- | Return the list of hs-source-dirs for all libraries and executables.
allLibExeHsSourceDirs :: CabalFile -> [FilePath]
allLibExeHsSourceDirs cabalFile =
  allHsSourceDirsForComponentType LibraryComponent cabalFile
  ++ allHsSourceDirsForComponentType ExecutableComponent cabalFile

-- | Return the list of hs-source-dirs for all test suites.
allTestHsSourceDirs :: CabalFile -> [FilePath]
allTestHsSourceDirs = allHsSourceDirsForComponentType TestSuiteComponent

-- | Return the list of hs-source-dirs for all benchmarks.
allBenchHsSourceDirs :: CabalFile -> [FilePath]
allBenchHsSourceDirs = allHsSourceDirsForComponentType BenchmarkComponent

-- | Return the list of hs-source-dirs for all foreign libraries.
allFLibHsSourceDirs :: CabalFile -> [FilePath]
allFLibHsSourceDirs = allHsSourceDirsForComponentType ForeignLibraryComponent

-- | List the names of all components.
allComponentNames :: CabalFile -> [CabalComponentName]
allComponentNames = M.keys . hsSourceDirsMap

-- | List all names of library/executable components.
libExeComponentNames :: CabalFile -> [CabalComponentName]
libExeComponentNames = filter isLibOrExe . allComponentNames

-- | List all names of test suite components.
testComponentNames :: CabalFile -> [CabalComponentName]
testComponentNames = filter isTestSuite . allComponentNames

-- | List all names of benchmark components.
benchComponentNames :: CabalFile -> [CabalComponentName]
benchComponentNames = filter isBenchmark . allComponentNames
