module User.Region (
    Region(..)
  ) where

import DB.Derive

data Region = REGION_SE
              | REGION_GB
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''Region)

