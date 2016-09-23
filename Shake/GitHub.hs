module Shake.GitHub where

import Development.Shake
import System.Exit
import System.Process

import Shake.Oracles

-- | Wrap an action around GitHub notifications of build status.
-- Only works if you are in a TeamCity CI build, and have set the GitHub
-- environment variable
withGitHub :: String -> Action a -> Action a
withGitHub name a = do
  gh <- askOracle (BuildGitHub ())
  tc <- askOracle (TeamCity ())
  case gh && tc of
    False -> a
    True -> do
      putNormal $ "## Notifying GitHub that " ++ name ++ " has started"
      liftIO $ notifyGitHub name "pending" ("Checking " ++ name)
      result <- actionOnException a (notifyGitHub name "error" $
                                     name ++ " failed")
      putNormal $ "## Notifying GitHub that " ++ name ++ " was successful"
      liftIO $ notifyGitHub name "success" $ name ++ " successful"
      return result
  where
    notifyGitHub :: String -> String -> String -> IO ()
    notifyGitHub context status message = do
      (ec,stdout,stderr) <- readProcessWithExitCode "build-scripts/github.sh"
                            [context, status, message] []
      case ec of
        ExitFailure _ -> do
          putStrLn $ "# ERROR: GitHub notification failed, stdout:" ++ stdout
          putStrLn $ "# ERROR: GitHub notification failed, stderr:" ++ stderr
        _ -> return ()
