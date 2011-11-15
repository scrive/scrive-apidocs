module Auth.DocumentAuthorization where

import MinutesTime
import Auth.DocumentPrivilege
import Util.SignatoryLinkUtils
import Data.Semantic

-- | Instances of this class determine whether an Authorization can
-- act on the document
class DocumentAuthorization a where
  -- | Can this authorization act on this document at this time and
  -- privilege the action falls under
  canDocument :: HasSignatoryLinks sls => a -> sls -> MinutesTime -> DocumentPrivilege -> Bool
  canDocument _ _ _ _ = False

-- standard recursive instances
instance DocumentAuthorization a => DocumentAuthorization (Maybe a) where
  canDocument Nothing _ _ _ = False
  canDocument (Just a) x y z = canDocument a x y z

instance DocumentAuthorization a => DocumentAuthorization (Either x a) where
  canDocument (Left _) _ _ _ = False
  canDocument (Right a) x y z = canDocument a x y z

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (Or a b) where
  canDocument (Or a b) x y z = canDocument a x y z || canDocument b x y z
