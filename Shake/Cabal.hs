module Shake.Cabal (CabalFile(packageId, allExtensions), parseCabalFile
                   ,allComponentNames
                   ,libExeComponentNames, testComponentNames, benchComponentNames
                   ,allHsSourceDirs, componentHsSourceDirs
                   ,libExeHsSourceDirs, allLibExeHsSourceDirs
                   ,testHsSourceDirs, allTestHsSourceDirs
                   ,benchHsSourceDirs, allBenchSourceDirs
                   ) where

import qualified Data.Map                                      as M
import           Data.Maybe
import           Data.Monoid
import           Distribution.PackageDescription               hiding
                                                               (allExtensions)
import qualified Distribution.PackageDescription               as PkgDesc
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.Text                             (display)
import           Distribution.Verbosity                        (normal)
import           Language.Haskell.Extension

import Shake.Utils

-- | A component name -> list of hs-source-dirs map.
type HsSourceDirsMap = M.Map String [FilePath]

-- | All data that we need from a .cabal file.
data CabalFile = CabalFile {
  -- | Package identifier of this package (e.g. 'foo-2.1').
  packageId             :: String,
  -- | List all extensions used by this package.
  allExtensions         :: [Extension],
  -- | hs-source-dirs of libraries and exes, grouped per
  -- component. The default library has name "".
  libExeHsSourceDirsMap :: HsSourceDirsMap,
  -- | hs-source-dirs of test suites.
  testHsSourceDirsMap   :: HsSourceDirsMap,
  -- | hs-source-dirs of benchmarks.
  benchHsSourceDirsMap  :: HsSourceDirsMap
  }

-- | Parse a .cabal file.
parseCabalFile :: FilePath -> IO CabalFile
parseCabalFile cabalFile = do
  pkgDesc <- flattenPackageDescription <$> readPackageDescription normal cabalFile
  let libExeBuildInfos =  [("", libBuildInfo $ lib)
                          | lib <- maybeToList $ library pkgDesc ]
                       ++ [(exeName exe, buildInfo exe)
                          | exe <- executables pkgDesc ]
      testBuildInfos   =  [(testName test, testBuildInfo test)
                          | test <- testSuites pkgDesc]
      benchBuildInfos  =  [(benchmarkName bench, benchmarkBuildInfo bench)
                          | bench <- benchmarks pkgDesc]
      srcDirs bis      =  M.fromList [(name, ordNub . hsSourceDirs $ bi)
                                     | (name, bi) <- bis]
      exts             = ordNub . concatMap PkgDesc.allExtensions . allBuildInfo $
                         pkgDesc
      pkgid            = display . package $ pkgDesc
  return $ CabalFile pkgid exts (srcDirs libExeBuildInfos) (srcDirs testBuildInfos)
                     (srcDirs benchBuildInfos)

componentHsSourceDirs' :: String -> HsSourceDirsMap -> [FilePath]
componentHsSourceDirs' compName = fromMaybe [] . M.lookup compName

-- | Return the list of hs-source-dirs for a given library or executable.
libExeHsSourceDirs :: String -> CabalFile -> [FilePath]
libExeHsSourceDirs compName cabalFile =
  componentHsSourceDirs' compName (libExeHsSourceDirsMap cabalFile)

-- | Return the list of hs-source-dirs for all libraries and executables.
allLibExeHsSourceDirs :: CabalFile -> [FilePath]
allLibExeHsSourceDirs = allHsSourceDirs' . libExeHsSourceDirsMap

-- | Return the list of hs-source-dirs for a given test suite.
testHsSourceDirs :: String -> CabalFile -> [FilePath]
testHsSourceDirs compName cabalFile =
  componentHsSourceDirs' compName (testHsSourceDirsMap cabalFile)

-- | Return the list of hs-source-dirs for all test suites.
allTestHsSourceDirs :: CabalFile -> [FilePath]
allTestHsSourceDirs = allHsSourceDirs' . testHsSourceDirsMap

-- | Return the list of hs-source-dirs for a given benchmark.
benchHsSourceDirs :: String -> CabalFile -> [FilePath]
benchHsSourceDirs compName cabalFile =
  componentHsSourceDirs' compName (benchHsSourceDirsMap cabalFile)

-- | Return the list of hs-source-dirs for all benchmarks.
allBenchSourceDirs :: CabalFile -> [FilePath]
allBenchSourceDirs = allHsSourceDirs' . benchHsSourceDirsMap

-- | Return the list of hs-source-dirs for a given component.
componentHsSourceDirs :: String -> CabalFile -> [FilePath]
componentHsSourceDirs compName cabalFile =
  fromMaybe [] . getFirst . foldMap nonEmpty $
    [libExeHsSourceDirs compName cabalFile
    ,testHsSourceDirs   compName cabalFile
    ,benchHsSourceDirs  compName cabalFile]
  where
    -- All components in Cabal must currently have unique names, but we handle
    -- the case of possible duplicates anyway, giving priority to
    -- libs/exes over test suites over benchmarks.
    nonEmpty [] = First Nothing
    nonEmpty xs = First (Just xs)

allHsSourceDirs' :: HsSourceDirsMap -> [FilePath]
allHsSourceDirs' = ordNub . concat . M.elems

-- | Return the list of hs-source-dirs for all components.
allHsSourceDirs :: CabalFile -> [FilePath]
allHsSourceDirs cabalFile = ordNub . concat $
  map allHsSourceDirs' [libExeHsSourceDirsMap cabalFile
                       ,testHsSourceDirsMap   cabalFile
                       ,benchHsSourceDirsMap  cabalFile]

componentNames :: HsSourceDirsMap -> [String]
componentNames = M.keys

-- | List all names of library/executable components.
libExeComponentNames :: CabalFile -> [String]
libExeComponentNames = componentNames . libExeHsSourceDirsMap

-- | List all names of test suite components.
testComponentNames :: CabalFile -> [String]
testComponentNames = componentNames . testHsSourceDirsMap

-- | List all names of benchmark components.
benchComponentNames :: CabalFile -> [String]
benchComponentNames = componentNames . benchHsSourceDirsMap

-- | List the names of all components.
allComponentNames :: CabalFile -> [String]
allComponentNames cabalFile = libExeComponentNames cabalFile
                              ++ testComponentNames   cabalFile
                              ++ benchComponentNames  cabalFile
