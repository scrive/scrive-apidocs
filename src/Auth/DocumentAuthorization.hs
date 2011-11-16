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
  -- this will probably go away and be replaced by "isExpired" or something like that
  canDocument :: HasSignatoryLinks sls => a -> sls -> MinutesTime -> DocumentPrivilege -> Bool
  canDocument _ _ _ _ = False
  
  docSqlWhere :: a -> DocumentPrivilege -> DocumentID -> (String, SqlValue)
  docSqlWhere _ _ _ = ("(1 = 2)", []) -- an expression that always fails

-- standard recursive instances
instance DocumentAuthorization a => DocumentAuthorization (Maybe a) where
  canDocument Nothing _ _ _ = False
  canDocument (Just a) x y z = canDocument a x y z
  
  docSqlWhere Nothing _ _ = ("(1 = 2)", [])
  docSqlWhere (Just a) p i = docSqlWhere a p i

instance DocumentAuthorization a => DocumentAuthorization (Either x a) where
  canDocument (Left _) _ _ _ = False
  canDocument (Right a) x y z = canDocument a x y z
  
  docSqlWhere (Left _) _ _ = ("(1 = 2)", [])
  docSqlWhere (Right a) p i = docSqlWhere a p i

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (Or a b) where
  canDocument (Or a b) x y z = canDocument a x y z || canDocument b x y z
  
  docSqlWhere (Or a b) p i = 
    let (s1, v1) = docSqlWhere a p i
        (s2, v2) = docSqlWhere b p i
    in ("( " ++ s1 ++ " OR " ++ s2 ++ " )", v1 ++ v2)
