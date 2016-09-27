module Partner.Partner
    ( PartnerID
    , Partner(..)
    , unsafePartnerID
    , unjsonPartner
    , unPartnerID    
    ) where
    
import Data.Unjson

import KontraPrelude
import Partner.PartnerID

data Partner = Partner {
      ptID :: !PartnerID
    , ptName :: !String
    , ptDefaultPartner :: !Bool
    } deriving (Eq, Ord, Show)

unjsonPartner :: UnjsonDef Partner
unjsonPartner = objectOf $ pure Partner
  <*> field "partnerid"
      ptID
      "Id of a partner"
  <*> fieldDef "partnername" ""
      ptName
      "Name of a partner"
  <*> fieldDef "defaultpartner" False
      ptDefaultPartner
      "Whether the partner is the default partner in the system"

instance Unjson Partner where
  unjsonDef = unjsonPartner
