module Partner.Partner
    ( PartnerID
    , Partner(..)
    , unsafePartnerID
    , unPartnerID
    ) where

import Partner.PartnerID
import UserGroup.Types

data Partner = Partner
  { ptID :: !PartnerID
  , ptName :: !String
  , ptDefaultPartner :: !Bool
  , ptUserGroupID :: Maybe UserGroupID
  } deriving (Eq, Ord, Show)
