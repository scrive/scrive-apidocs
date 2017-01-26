module Shake.Cabal (parseCabalFile
                   ,HsSourceDirs, getHsSourceDirs
                   ,allComponentNames
                   ,libExeComponentNames, testComponentNames, benchComponentNames
                   ,allHsSourceDirs, componentHsSourceDirs
                   ,libExeHsSourceDirs, testHsSourceDirs, benchHsSourceDirs
                   ,allExtensions
                   ) where

import qualified Data.Map                                      as M
import           Data.Maybe
import           Data.Monoid
import           Distribution.PackageDescription               hiding
                                                               (hsSourceDirs
                                                               ,allExtensions)
import qualified Distribution.PackageDescription               as PkgDesc
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity                        (normal)
import           Language.Haskell.Extension

import Shake.Utils

-- | A component name -> list of hs-source-dirs map.
type HsSourceDirsMap = M.Map String [FilePath]

-- | All HsSourceDirsMaps in a package description, grouped by
-- component type. Default library in 'libExeHsSourceDirs' has an
-- empty component name.
data HsSourceDirs = HsSourceDirs {
  libExeHsSourceDirsMap :: HsSourceDirsMap,
  testHsSourceDirsMap   :: HsSourceDirsMap,
  benchHsSourceDirsMap  :: HsSourceDirsMap
  }

-- | Parse a .cabal file.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile cabalFile =
  flattenPackageDescription <$> readPackageDescription normal cabalFile

-- | Given a parsed .cabal file, return a 'HsSourceDirs' record.
getHsSourceDirs :: PackageDescription -> IO HsSourceDirs
getHsSourceDirs pkgDesc = do
  let libExeBuildInfos =  [("", libBuildInfo $ lib)
                          | lib <- maybeToList $ library pkgDesc ]
                       ++ [(exeName exe, buildInfo exe)
                          | exe <- executables pkgDesc ]
      testBuildInfos   =  [(testName test, testBuildInfo test)
                          | test <- testSuites pkgDesc]
      benchBuildInfos  =  [(benchmarkName bench, benchmarkBuildInfo bench)
                          | bench <- benchmarks pkgDesc]
      srcDirs bis      =  M.fromList [(name, ordNub . PkgDesc.hsSourceDirs $ bi)
                                     | (name, bi) <- bis]
  return $! HsSourceDirs (srcDirs libExeBuildInfos) (srcDirs testBuildInfos)
                         (srcDirs benchBuildInfos)

componentHsSourceDirs' :: String -> HsSourceDirsMap -> [FilePath]
componentHsSourceDirs' compName = fromMaybe [] . M.lookup compName

-- | Return the list of hs-source-dirs for a given library or executable.
libExeHsSourceDirs :: String -> HsSourceDirs -> [FilePath]
libExeHsSourceDirs compName hsSourceDirs =
  componentHsSourceDirs' compName (libExeHsSourceDirsMap hsSourceDirs)

-- | Return the list of hs-source-dirs for a given test suite.
testHsSourceDirs :: String -> HsSourceDirs -> [FilePath]
testHsSourceDirs compName hsSourceDirs =
  componentHsSourceDirs' compName (testHsSourceDirsMap hsSourceDirs)

-- | Return the list of hs-source-dirs for a given benchmark.
benchHsSourceDirs :: String -> HsSourceDirs -> [FilePath]
benchHsSourceDirs compName hsSourceDirs =
  componentHsSourceDirs' compName (benchHsSourceDirsMap hsSourceDirs)

-- | Return the list of hs-source-dirs for a given component.
componentHsSourceDirs :: String -> HsSourceDirs -> [FilePath]
componentHsSourceDirs compName hsSourceDirs =
  fromMaybe [] . getFirst . foldMap nonEmpty $
    [libExeHsSourceDirs compName hsSourceDirs
    ,testHsSourceDirs   compName hsSourceDirs
    ,benchHsSourceDirs  compName hsSourceDirs]
  where
    -- All components in Cabal must currently have unique names, but we handle
    -- the case of possible duplicates anyway, giving priority to
    -- libs/exes over test suites over benchmarks.
    nonEmpty [] = First Nothing
    nonEmpty xs = First (Just xs)

-- | Return the list of hs-source-dirs for all components.
allHsSourceDirs :: HsSourceDirs -> [FilePath]
allHsSourceDirs hsSourceDirs = ordNub . concat $
  map toDirsList [libExeHsSourceDirsMap hsSourceDirs
                 ,testHsSourceDirsMap   hsSourceDirs
                 ,benchHsSourceDirsMap  hsSourceDirs]
  where
    toDirsList :: HsSourceDirsMap -> [FilePath]
    toDirsList hsSrcDirsMap = ordNub . concat . M.elems $ hsSrcDirsMap

componentNames :: HsSourceDirsMap -> [String]
componentNames = M.keys

-- | List all names of library/executable components.
libExeComponentNames :: HsSourceDirs -> [String]
libExeComponentNames = componentNames . libExeHsSourceDirsMap

-- | List all names of test suite components.
testComponentNames :: HsSourceDirs -> [String]
testComponentNames = componentNames . testHsSourceDirsMap

-- | List all names of benchmark components.
benchComponentNames :: HsSourceDirs -> [String]
benchComponentNames = componentNames . benchHsSourceDirsMap

-- | List the names of all components.
allComponentNames :: HsSourceDirs -> [String]
allComponentNames hsSourceDirs = libExeComponentNames hsSourceDirs
                              ++ testComponentNames   hsSourceDirs
                              ++ benchComponentNames  hsSourceDirs

-- | List all extensions used by this package.
allExtensions :: PackageDescription -> [Extension]
allExtensions pkgDesc = ordNub. concatMap PkgDesc.allExtensions . allBuildInfo $
                        pkgDesc
