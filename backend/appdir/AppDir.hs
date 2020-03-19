module AppDir
  ( AppPaths (..)
  , setupAppPaths
  )
where

import Data.Maybe (fromMaybe)
import System.Directory (setCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (FilePath)

data AppPaths = AppPaths
  -- The project root directory for reading file assets.
  { appSourceRoot :: FilePath

  -- The workspace directory for reading config files and writing outputs.
  , appWorkspaceRoot :: FilePath
  }

-- When starting a main program, this function should be called to set the
-- current directory to the environment variable $KONTRAKCJA_ROOT if available.
-- This is needed as many part of the program currently read files relative to
-- current directory, assuming that it is the same as the project root
-- directory. This breaks the app if it is not running from the project root
-- directory, i.e. from the new workspace directories. The function also
-- returns the values of the environment variables $KONTRAKCJA_ROOT and
-- $KONTRAKCJA_WORKSPACE to be used by the caller.
--
-- While it is still possible to cd to the directory before running the
-- executables, this does not work when we want to run `cabal v2-run` from
-- the workspace directories, since cabal reads from the current directory.
-- Having the environment variables also makes it easier to deploy kontrakcja
-- as services such as systemd, since we can put the assets and configurations
-- in separate directories without the source code and set the environment
-- variables in the service configuration.
setupAppPaths :: IO AppPaths
setupAppPaths = do
  sourceRoot    <- fromMaybe "." <$> lookupEnv "KONTRAKCJA_ROOT"
  workspaceRoot <- fromMaybe sourceRoot <$> lookupEnv "KONTRAKCJA_WORKSPACE"

  putStrLn $ "KONTRAKCJA_ROOT: " <> sourceRoot
  putStrLn $ "KONTRAKCJA_WORKSPACE: " <> workspaceRoot

  setCurrentDirectory sourceRoot

  return $ AppPaths sourceRoot workspaceRoot
