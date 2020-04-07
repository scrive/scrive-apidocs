module Shake.Utils
  ( (%>>>)
  , mapP_
  , getHsDeps
  , langEnv
  , findWithDefault
  , needPatternsInDirectories
  , ordNub
  ) where

import Control.Monad
import Data.Maybe
import Development.Shake
import System.FilePath ((</>))
import qualified Data.Set as Set

-- * Utilities

-- | Parallel version of 'mapM_'.
mapP_ :: (a -> Action r) -> [a] -> Action ()
mapP_ f xs = void $ forP xs f

-- List all Haskell source files within a directory,
-- identified by the ".hs" extension.
getHsDeps :: FilePath -> IO [FilePath]
getHsDeps rootPath = do
  files <- getDirectoryFilesIO rootPath ["//*.hs"]
  return $ map (rootPath </>) files

-- | Perform a `need` on all files matching the given [FilePattern] in
-- the given [FilePath] For each directory we get all the files using
-- `getDirectoryFiles` that match the pattern and apply a `need` on
-- it, this also tracks the result of `getDirectoryFiles` (for
-- matching files)
needPatternsInDirectories :: FilePath -> [FilePattern] -> [FilePath] -> Action ()
needPatternsInDirectories sourceRoot pats dirs = do
  forM_
    dirs
    (\d -> do
      hs <- getDirectoryFiles (sourceRoot </> d) pats
      need $ map (\f -> sourceRoot </> d </> f) hs
    )

infix 1 %>>>

-- | Used to wrap rules where the Stdout of the last command is written to the
-- relevant file.
-- Replaces usage of
-- >> "foo.*" %> \f -> do Stdout o <- cmd "foo"; liftIO $ writeFile f o
--
-- Though be sure to include `EchoStdout True` in the last command if this
-- behaviour is desired
(%>>>) :: FilePattern -> Action (Stdout String) -> Rules ()
(%>>>) fp a = fp %> \t -> do
  Stdout out <- a
  liftIO $ writeFile t out

langEnv :: [CmdOption]
langEnv = [AddEnv "LANG" "en_US.UTF-8", AddEnv "LC_ALL" "en_US.UTF-8"]

ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ []       = []
    go s (x : xs) = if x `Set.member` s then go s xs else x : go (Set.insert x s) xs

findWithDefault :: Eq key => val -> key -> [(key, val)] -> val
findWithDefault def k = fromMaybe def . lookup k
