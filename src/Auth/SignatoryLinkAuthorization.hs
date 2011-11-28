module Auth.SignatoryLinkAuthorization where

import MinutesTime
import Auth.SignatoryLinkPrivilege
--import Util.SignatoryLinkUtils
import Data.Semantic
import SQL.Builder
import Doc.DocState

-- | Instances of this class determine whether an Authorization can
-- act on a signatory link
class SignatoryLinkAuthorization a where
  sigSqlWhere :: a -> SignatoryLinkPrivilege -> SignatoryLinkID -> DocumentID -> MinutesTime -> WhereClause
  sigSqlWhere _ _ _ _ _ = WhereSimple "(1 = 2)" [] -- an expression that always fails

-- standard recursive instances
instance SignatoryLinkAuthorization a => SignatoryLinkAuthorization (Maybe a) where
  sigSqlWhere Nothing _ _ _ _ = WhereSimple "(1 = 2)" []
  sigSqlWhere (Just a) p s i mt = sigSqlWhere a p s i mt

instance SignatoryLinkAuthorization a => SignatoryLinkAuthorization (Either x a) where
  sigSqlWhere (Left _) _ _ _ _ = WhereSimple "(1 = 2)" []
  sigSqlWhere (Right a) p s i mt = sigSqlWhere a p s i mt

instance (SignatoryLinkAuthorization a, SignatoryLinkAuthorization b) => SignatoryLinkAuthorization (Or a b) where
  sigSqlWhere (Or a b) p s i mt = WhereOr (sigSqlWhere a p s i mt) (sigSqlWhere b p s i mt)

instance (SignatoryLinkAuthorization a, SignatoryLinkAuthorization b) => SignatoryLinkAuthorization (And a b) where
  sigSqlWhere (And a b) p s i mt = WhereAnd (sigSqlWhere a p s i mt) (sigSqlWhere b p s i mt)
