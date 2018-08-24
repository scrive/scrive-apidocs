module Shake.GetHsDeps (getHsDeps)
  where

import Data.List
import Development.Shake.Util (parseMakefile)
import System.IO.Extra        (readFile', withTempDir)
import System.FilePath        ((</>))
import System.Process         (callProcess)

-- | Given a Haskell source file name, return a list of all local
-- Haskell source files it depends on, as given by 'ghc -M'.
getHsDeps :: FilePath -> IO [FilePath]
getHsDeps mainIs = do
  withTempDir $ \dir -> do
    let tmpFile = dir </> ".depend"
    callProcess "ghc"
      [ "-hide-all-packages"

        -- We use 'getHsDeps' only on 'Shake/Shake.hs', so this is
        -- fine. Otherwise GHC would get confused by the package
        -- environment file.
      , "-package", "Cabal"
      , "-package", "aeson"
      , "-package", "attoparsec"
      , "-package", "base"
      , "-package", "bytestring"
      , "-package", "containers"
      , "-package", "directory"
      , "-package", "extra"
      , "-package", "filepath"
      , "-package", "pretty"
      , "-package", "process"
      , "-package", "shake"
      , "-package", "text"
      , "-package", "time"
      , "-package", "unordered-containers"

      , "-dep-suffix", "", "-M", mainIs
      , "-dep-makefile", tmpFile ]
    deps <- nub . filter (".hs" `isSuffixOf`)
      . concatMap snd . parseMakefile <$> readFile' tmpFile
    return deps
