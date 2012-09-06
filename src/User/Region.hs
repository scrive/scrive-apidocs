module User.Region (
    Region(..)
  , codeFromRegion
  , regionFromCode
  , defaultRegionLang
  , regionFromHTTPHeader
  ) where

import Data.Foldable hiding (find)
import Data.List
import Data.Maybe
import DB.Derive
import Utils.Enum
import Utils.List
import User.Lang
import Data.Char

data Region = REGION_SE
              | REGION_GB
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Region)

codeFromRegion :: Region -> String
codeFromRegion REGION_SE = "se"
codeFromRegion REGION_GB = "gb"

regionFromCode :: String -> Maybe Region
regionFromCode s = find ((== map toLower s) . codeFromRegion) allValues

defaultRegionLang :: Region -> Lang
defaultRegionLang REGION_SE = LANG_SE
defaultRegionLang REGION_GB = LANG_EN

regionFromHTTPHeader :: String -> Region
regionFromHTTPHeader s = fromMaybe REGION_GB $ msum $ map findRegion (splitOver "," s)
  where
    findRegion str = find ((`isInfixOf` str) . codeFromLang . defaultRegionLang) allValues
