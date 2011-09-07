module Doc.DocRegion (
    DocRegionInfo(..)
  , getRegionInfo)
where

import Doc.DocStateData
import User.Region


class HasRegion a where
  getRegionInfo :: a -> DocRegionInfo

instance HasRegion Region where
  getRegionInfo REGION_SE = swedishRegionInfo
  getRegionInfo REGION_GB = britishRegionInfo

instance HasRegion Document where
  getRegionInfo = getRegionInfo . documentregion

data DocRegionInfo = DocRegionInfo {
  regionhaspeopleids :: Bool
, regionelegavailable :: Bool
}

swedishRegionInfo :: DocRegionInfo
swedishRegionInfo =
  DocRegionInfo {
    regionhaspeopleids = True
  , regionelegavailable = True
  }

britishRegionInfo :: DocRegionInfo
britishRegionInfo =
  DocRegionInfo {
    regionhaspeopleids = False
  , regionelegavailable = False
  }

