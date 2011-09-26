module User.Region (
    Region(..)
  , codeFromRegion
  , regionFromCode
  , defaultRegionLang
  ) where

import Data.List
import DB.Derive
import Misc
import User.Lang

data Region = REGION_SE
              | REGION_GB
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Region)

codeFromRegion :: Region -> String
codeFromRegion REGION_SE = "se"
codeFromRegion REGION_GB = "gb"

regionFromCode :: String -> Maybe Region
regionFromCode s = find ((== s) . codeFromRegion) allValues

defaultRegionLang :: Region -> Lang
defaultRegionLang REGION_SE = LANG_SE
defaultRegionLang REGION_GB = LANG_EN

