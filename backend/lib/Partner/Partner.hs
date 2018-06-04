module Partner.Partner
    ( PartnerID
    , Partner(..)
    , unsafePartnerID
    , unPartnerID
    ) where

import Partner.PartnerID
import UserGroup.Data

data Partner = Partner {
      ptID :: !PartnerID
    , ptName :: !String
    , ptDefaultPartner :: !Bool
    , ptUserGroupID :: Maybe UserGroupID
    } deriving (Eq, Ord, Show)
