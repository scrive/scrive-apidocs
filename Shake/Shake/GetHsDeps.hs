module Shake.GetHsDeps (getHsDeps)
  where

import Development.Shake (getDirectoryFilesIO)
import System.FilePath ((</>))

-- List all Haskell source files within a directory,
-- identified by the ".hs" extension.
getHsDeps :: FilePath -> IO [FilePath]
getHsDeps rootPath = do
  files <- getDirectoryFilesIO rootPath ["//*.hs"]
  return $ fmap ((</>) rootPath) files
