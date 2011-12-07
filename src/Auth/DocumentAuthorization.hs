module Auth.DocumentAuthorization where

import MinutesTime
import Auth.DocumentPrivilege
--import Util.SignatoryLinkUtils
import Data.Semantic
import SQL.Builder
import Doc.DocState

-- | Instances of this class determine whether an Authorization can
-- act on the document
class DocumentAuthorization a where
  docSqlWhere :: a -> DocumentPrivilege -> DocumentID -> MinutesTime -> WhereClause
  docSqlWhere _ _ _ _ = WhereSimple "(1 = 2)" [] -- an expression that always fails

-- standard recursive instances
instance DocumentAuthorization a => DocumentAuthorization (Maybe a) where
  docSqlWhere Nothing _ _ _ = WhereSimple "(1 = 2)" []
  docSqlWhere (Just a) p i mt = docSqlWhere a p i mt

instance DocumentAuthorization a => DocumentAuthorization (Either x a) where
  docSqlWhere (Left _) _ _ _ = WhereSimple "(1 = 2)" []
  docSqlWhere (Right a) p i mt = docSqlWhere a p i mt

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (Or a b) where
  docSqlWhere (Or a b) p i mt = WhereOr (docSqlWhere a p i mt) (docSqlWhere b p i mt)

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (And a b) where
  docSqlWhere (And a b) p i mt = WhereAnd (docSqlWhere a p i mt) (docSqlWhere b p i mt)
