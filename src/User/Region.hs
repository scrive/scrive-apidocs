module User.Region (
    Region(..)
  , codeFromRegion
  , regionFromCode
  ) where

import Data.List
import DB.Derive
import Misc

data Region = REGION_SE
              | REGION_GB
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Region)

codeFromRegion :: Region -> String
codeFromRegion REGION_SE = "se"
codeFromRegion REGION_GB = "gb"

regionFromCode :: String -> Maybe Region
regionFromCode s = find ((== s) . codeFromRegion) allValues

