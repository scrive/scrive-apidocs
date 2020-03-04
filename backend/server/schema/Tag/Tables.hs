module Tag.Tables where

import DB

-- | Tags can be attached to some objects (currently users and user groups).
-- Data is stored in separate tables (see `tableUserTags` and `tableUserGroupTags`)
-- and this composite type is used when querying those.
ctTag :: CompositeType
ctTag = CompositeType
  { ctName    = "tag_c1"
  , ctColumns = [ CompositeColumn { ccName = "name", ccType = TextT }
                , CompositeColumn { ccName = "value", ccType = TextT }
                ]
  }
