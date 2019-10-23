module Version (genVersionID) where

import Control.Exception.Lifted as E
import System.Environment

genVersionID :: IO String
genVersionID = do
  let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
      catchIO = E.catch
  buildNumber    <- catchIO (getEnv "BUILD_NUMBER") (const (return "build_number"))
  buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER")
                            (const (return "build_vcs_number"))
  buildDate <- catchIO (getEnv "BUILD_DATE") (const (return "build_date"))

  return $ buildDate ++ "." ++ buildNumber ++ "." ++ buildVcsNumber
