module Shake.Cabal (parseCabalFile
                   ,HsSourceDirsMap
                   ,getHsSourceDirs
                   ,allHsSourceDirs
                   ,componentHsSourceDirs) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)

type HsSourceDirsMap = M.Map String [FilePath]

-- | Parse a .cabal file.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile cabalFile =
  flattenPackageDescription <$> readPackageDescription normal cabalFile

-- | Given a parsed .cabal file, return a component name -> list of
-- hs-source-dirs map. Default library has an empty component name.
getHsSourceDirs :: PackageDescription -> IO HsSourceDirsMap
getHsSourceDirs pkgDesc = do
  let buildInfos =  [("", libBuildInfo $ lib)
                    | lib <- maybeToList $ library pkgDesc ]
                 ++ [(exeName exe, buildInfo exe)
                    | exe <- executables pkgDesc ]
                 ++ [(testName test, testBuildInfo test)
                    | test <- testSuites pkgDesc]
                 ++ [(benchmarkName bench, benchmarkBuildInfo bench)
                    | bench <- benchmarks pkgDesc]
      srcDirs    =  [(name, nub . hsSourceDirs $ bi) | (name, bi) <- buildInfos]
  return . M.fromList $ srcDirs

allHsSourceDirs :: HsSourceDirsMap -> [FilePath]
allHsSourceDirs hsSrcDirsMap = nub . concat . M.elems $ hsSrcDirsMap

componentHsSourceDirs :: String -> HsSourceDirsMap -> [FilePath]
componentHsSourceDirs compName = fromMaybe [] . M.lookup compName
