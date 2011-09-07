module User.SystemServer (
    SystemServer(..)
  , systemServerFromURL
  , defaultLang
  , defaultRegion
  , targetedLocales
  ) where

import Data.List
import DB.Derive
import User.Lang
import User.Region

data SystemServer = SkrivaPa | Scrive
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''SystemServer)

systemServerFromURL :: String -> SystemServer
systemServerFromURL url =  if ("scrive" `isInfixOf` url)
                             then Scrive
                             else SkrivaPa

defaultLang :: SystemServer -> Lang
defaultLang SkrivaPa = LANG_SE
defaultLang Scrive = LANG_EN

defaultRegion :: SystemServer -> Region
defaultRegion SkrivaPa = REGION_SE
defaultRegion Scrive = REGION_GB

targetedLocales :: SystemServer -> [(Region, Lang)]
targetedLocales SkrivaPa = [(REGION_SE, LANG_SE)]
targetedLocales Scrive = [(REGION_GB, LANG_EN)]

