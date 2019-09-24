module Shake.GetHsDeps (getHsDeps)
  where

import Data.List
import Development.Shake.Util (parseMakefile)
import System.FilePath ((</>))
import System.IO.Extra (readFile', withTempDir)
import System.Process (callProcess)

-- | Given a Haskell source file name, return a list of all local
-- Haskell source files it depends on, as given by 'ghc -M'.
getHsDeps :: FilePath -> FilePath -> IO [FilePath]
getHsDeps rootPath mainIs = do
  withTempDir $ \dir -> do
    let tmpFile = dir </> ".depend"
    callProcess "ghc"
      [ "-i" <> rootPath
      , "-hide-all-packages"

        -- We use 'getHsDeps' only on 'Shake/Shake.hs', so this is
        -- fine. Otherwise GHC would get confused by the package
        -- environment file.
        --
        -- The list of packages in scope MUST be kept in sync with the
        -- one in shake.sh.
      , "-package", "Cabal"
      , "-package", "aeson"
      , "-package", "attoparsec"
      , "-package", "base"
      , "-package", "bytestring"
      , "-package", "containers"
      , "-package", "directory"
      , "-package", "extra"
      , "-package", "filepath"
      , "-package", "mtl"
      , "-package", "pretty"
      , "-package", "process"
      , "-package", "shake"
      , "-package", "text"
      , "-package", "time"
      , "-package", "unordered-containers"

      , "-dep-suffix", ""
      , "-M", rootPath </> mainIs
      , "-dep-makefile", tmpFile ]
    deps <- nub . filter (".hs" `isSuffixOf`)
      . concatMap snd . parseMakefile <$> readFile' tmpFile
    return deps
