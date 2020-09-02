{-# LANGUAGE NoDuplicateRecordFields #-}
module Version
  ( BuildVersion(..)
  , asVersionId
  , genBuildVersion
  )
where

import Control.Exception.Lifted as E
import System.Environment

data BuildVersion = BuildVersion
  { buildDate :: String
  , buildNumber :: String
  , buildVcsNumber :: String
  }

genBuildVersion :: IO BuildVersion
genBuildVersion = do
  let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
      catchIO = E.catch
  buildNumber    <- catchIO (getEnv "BUILD_NUMBER") (const (return "build_number"))
  buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER")
                            (const (return "build_vcs_number"))
  buildDate <- catchIO (getEnv "BUILD_DATE") (const (return "build_date"))
  pure BuildVersion { .. }

asVersionId :: BuildVersion -> String
asVersionId BuildVersion {..} = buildDate <> "." <> buildNumber <> "." <> buildVcsNumber
