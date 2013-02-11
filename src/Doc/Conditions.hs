

module Doc.Conditions
where

import Doc.DocStateData
import DB.SQL2
import Control.Exception.Lifted
import Control.Monad.State.Class
import Data.Typeable
import DB.SQL
import Data.List
import Company.CompanyID
import User.UserID
import Doc.DocumentID

-- This is the part where we define all possible wrongs about a document.


data DocumentTypeShouldBe = DocumentTypeShouldBe { documentTypeShouldBe :: DocumentType
                                                 , documentTypeIs :: DocumentType
                                                 }
                      deriving (Eq, Ord, Typeable)

instance Show DocumentTypeShouldBe where
  show (DocumentTypeShouldBe a b) = "DocumentTypeShouldBe { documentTypeShouldBe = " ++ x a ++ ", documentTypeIs = " ++ x b ++ " }"
    where x (Signable {}) = "Signable"
          x (Template {}) = "Template"


instance Exception DocumentTypeShouldBe

sqlWhereDocumentTypeIs :: (MonadState v m, SqlWhere v)
                       => DocumentType -> m ()
sqlWhereDocumentTypeIs xtype = sqlWhereEV (DocumentTypeShouldBe xtype, "documents.type") $ "documents.type =" <?> xtype

data DocumentStatusShouldBe = DocumentStatusShouldBe { documentStatusShouldBeDocumentID :: DocumentID
                                                     , documentStatusShouldBe :: [DocumentStatus]
                                                     , documentStatusIs :: DocumentStatus
                                                     }
                      deriving (Eq, Ord, Typeable)

instance Show DocumentStatusShouldBe where
  show (DocumentStatusShouldBe d a b) = "DocumentStatusShouldBe { documentStatusShouldBeDocumentID = " ++ show d ++ ", documentStatusShouldBe = [" ++ intercalate "," (map x a) ++ "], documentStatusIs = " ++ x b ++ " }"
    where x (DocumentError {}) = "DocumentError {}"
          x v = show v

instance Exception DocumentStatusShouldBe

sqlWhereDocumentStatusIsOneOf :: (MonadState v m, SqlWhere v)
                              => [DocumentStatus] -> m ()
sqlWhereDocumentStatusIsOneOf [] =
  sqlWhereEVV (\d -> DocumentStatusShouldBe d [], "documents.id", "documents.status") "FALSE"
sqlWhereDocumentStatusIsOneOf [s] =
  sqlWhereEVV (\d -> DocumentStatusShouldBe d [s], "documents.id", "documents.status") ("documents.status = " <?> s)
sqlWhereDocumentStatusIsOneOf sx =
  sqlWhereEVV (\d -> DocumentStatusShouldBe d sx, "documents.id", "documents.status") ("documents.status IN" <+> parenthesize (sqlConcatComma (map sqlParam sx)))

sqlWhereDocumentStatusIs :: (MonadState v m, SqlWhere v)
                         => DocumentStatus -> m ()
sqlWhereDocumentStatusIs status =
  sqlWhereDocumentStatusIsOneOf [status]


data UserShouldBeSelfOrCompanyAdmin = UserShouldBeSelfOrCompanyAdmin
  { userShouldBeSelfOrCompanyAdminUserID    :: UserID
  , userShouldBeSelfOrCompanyAdminUserEmail :: String
  , userShouldBeSelfOrCompanyAdminCompanyID :: CompanyID
  }
     deriving (Eq, Ord, Typeable, Show)

instance Exception UserShouldBeSelfOrCompanyAdmin

sqlWhereUserIsSelfOrCompanyAdmin :: (MonadState v m, SqlWhere v)
                                 => m ()
sqlWhereUserIsSelfOrCompanyAdmin =
  sqlWhereEVVV (UserShouldBeSelfOrCompanyAdmin,"same_company_users.id","same_company_users.email",
                "same_company_users.company_id")
              "(users.id = same_company_users.id OR same_company_users.is_company_admin)"

data UserShouldBeDirectlyOrIndirectlyRelatedToDocument = UserShouldBeDirectlyOrIndirectlyRelatedToDocument
  { userShouldBeDirectlyOrIndirectlyRelatedToDocumentUserID :: UserID
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentDocumentID :: DocumentID
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentDocumentTitle :: String
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentUserEmail :: String
  }
   deriving (Eq, Ord, Typeable, Show)

instance Exception UserShouldBeDirectlyOrIndirectlyRelatedToDocument

sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument :: (MonadState v m, SqlWhere v)
                                                    => UserID -> m ()
sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid =
  sqlWhereEVVV (UserShouldBeDirectlyOrIndirectlyRelatedToDocument uid, "(SELECT signatory_links.document_id)",
               "(SELECT documents.title FROM documents WHERE documents.id = signatory_links.document_id)",
               "(SELECT email FROM users WHERE id = " <?> uid <> ")")
              ("same_company_users.id = " <?> uid)

data DocumentShouldHaveAtLeastOneSignatoryLink = DocumentShouldHaveAtLeastOneSignatoryLink DocumentID
  deriving (Eq, Ord, Show, Typeable)

instance Exception DocumentShouldHaveAtLeastOneSignatoryLink

data DocumentDoesNotExist = DocumentDoesNotExist DocumentID
  deriving (Eq, Ord, Show, Typeable)

instance Exception DocumentDoesNotExist

sqlWhereDocumentIDIs :: (MonadState v m, SqlWhere v)
                     => DocumentID -> m ()
sqlWhereDocumentIDIs did =
  sqlWhereE (DocumentDoesNotExist did) ("documents.id = " <?> did)
