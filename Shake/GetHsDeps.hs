{-# LANGUAGE CPP #-}
module Shake.GetHsDeps (getHsDeps)
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

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
    callProcess "ghc" ["-dep-suffix", "", "-M", mainIs, "-dep-makefile", tmpFile]
    deps <- nub . filter (".hs" `isSuffixOf`)
      . concatMap snd . parseMakefile <$> readFile' tmpFile
    return deps
