{-# LANGUAGE NoDuplicateRecordFields #-}
module Version
  ( BuildVersion(..)
  , asVersionId
  , genBuildVersion
  )
where

import Control.Exception.Lifted as E
import Data.Aeson (defaultOptions, genericParseJSON)
import Data.Yaml
import GHC.Generics
import System.Environment

data BuildVersion = BuildVersion
  { buildDate :: String
  , buildNumber :: String
  , buildVcsNumber :: String
  , buildFlowApiVersion :: String
  }

newtype Spec = Spec { info :: Info }
  deriving (Generic)

instance FromJSON Spec where
  parseJSON = genericParseJSON defaultOptions

newtype Info = Info { version :: String }
  deriving (Generic)

instance FromJSON Info where
  parseJSON = genericParseJSON defaultOptions

readFlowApiVersion :: IO String
readFlowApiVersion = do
  result <- decodeFileEither "backend/flow/docs/api.yaml"
  pure $ either (const "flow_api_version") (version . info) result

genBuildVersion :: IO BuildVersion
genBuildVersion = do
  let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
      catchIO = E.catch
  buildNumber    <- catchIO (getEnv "BUILD_NUMBER") (const (return "build_number"))
  buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER")
                            (const (return "build_vcs_number"))
  buildDate           <- catchIO (getEnv "BUILD_DATE") (const (return "build_date"))
  buildFlowApiVersion <- catchIO readFlowApiVersion (const (return "flow_api_version"))
  pure BuildVersion { .. }

asVersionId :: BuildVersion -> String
asVersionId BuildVersion {..} = buildDate <> "." <> buildNumber <> "." <> buildVcsNumber
