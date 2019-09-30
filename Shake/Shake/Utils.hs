module Shake.Utils ((%>>>)
                   ,getHsDeps
                   ,langEnv
                   ,findWithDefault
                   ,needPatternsInDirectories
                   ,ordNub) where

import Control.Monad
import Data.Maybe
import Development.Shake
import System.FilePath ((</>))
import qualified Data.Set as Set

-- * Utilities

-- List all Haskell source files within a directory,
-- identified by the ".hs" extension.
getHsDeps :: FilePath -> IO [FilePath]
getHsDeps rootPath = do
  files <- getDirectoryFilesIO rootPath ["//*.hs"]
  return $ fmap ((</>) rootPath) files

-- | Perform a `need` on all files matching the given [FilePattern] in
-- the given [FilePath] For each directory we get all the files using
-- `getDirectoryFiles` that match the pattern and apply a `need` on
-- it, this also tracks the result of `getDirectoryFiles` (for
-- matching files)
needPatternsInDirectories :: [FilePattern] -> [FilePath] -> Action ()
needPatternsInDirectories pats dirs = do
  forM_ dirs ( \d -> do
    hs <- getDirectoryFiles d pats
    need $ map (\f -> d ++ "/" ++ f) hs
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
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

findWithDefault :: Eq key => val -> key -> [(key,val)] -> val
findWithDefault def k = fromMaybe def . lookup k
