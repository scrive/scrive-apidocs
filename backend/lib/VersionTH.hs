{-# LANGUAGE TemplateHaskell #-}
module VersionTH
  ( buildVersion
  , versionID
  ) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Version

{-# NOINLINE buildVersion #-}
-- brittany-disable-next-binding
buildVersion :: BuildVersion
buildVersion = $( do
  BuildVersion {..} <- runIO genBuildVersion
  buildDateName <- fromJust <$> lookupValueName "buildDate"
  buildDateLit <- litE $ stringL buildDate
  buildNumberName <- fromJust <$> lookupValueName "buildNumber"
  buildNumberLit <- litE $ stringL buildNumber
  buildVcsNumberName <- fromJust <$> lookupValueName "buildVcsNumber"
  buildVcsNumberLit <- litE $ stringL buildVcsNumber
  recConE 'BuildVersion
    [ pure (buildDateName, buildDateLit)
    , pure (buildNumberName, buildNumberLit)
    , pure (buildVcsNumberName, buildVcsNumberLit)
    ]
  )

versionID :: String
versionID = asVersionId buildVersion
