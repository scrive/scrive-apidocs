

module Doc.Conditions
where

import Doc.DocStateData
import DB.SQL2
import Control.Monad.State.Class
import Data.Typeable
import DB.SQL
import Data.List
import Company.CompanyID
import User.UserID
import Doc.DocumentID
import Doc.SignatoryLinkID
import Text.JSON.Gen
import MagicHash

-- This is the part where we define all possible wrongs about a document.


data DocumentTypeShouldBe = DocumentTypeShouldBe { documentTypeShouldBe :: DocumentType
                                                 , documentTypeIs :: DocumentType
                                                 }
                      deriving (Eq, Ord, Typeable)

instance Show DocumentTypeShouldBe where
  show (DocumentTypeShouldBe a b) = "DocumentTypeShouldBe { documentTypeShouldBe = " ++ x a ++ ", documentTypeIs = " ++ x b ++ " }"
    where x (Signable {}) = "Signable"
          x (Template {}) = "Template"

instance ToJSValue DocumentTypeShouldBe where
  toJSValue (DocumentTypeShouldBe a b) = runJSONGen $ do
                     value "message" ("Document has incorrect type" :: String)
                     value "expected" (x a)
                     value "actual" (x b)
    where x (Signable {}) = ("Signable" :: String)
          x (Template {}) = "Template"


instance KontraException DocumentTypeShouldBe

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

instance ToJSValue DocumentStatusShouldBe where
  toJSValue (DocumentStatusShouldBe d a b) = runJSONGen $ do
                     value "message" ("Document status is incorrect" :: String)
                     value "document_id" (show d)
                     value "expected" (map x a)
                     value "actual" (x b)
    where x (DocumentError {}) = "DocumentError {}"
          x v = show v

instance KontraException DocumentStatusShouldBe

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

instance ToJSValue UserShouldBeSelfOrCompanyAdmin where
  toJSValue (UserShouldBeSelfOrCompanyAdmin d a b) = runJSONGen $ do
                     value "message" ("User is not company admin" :: String)
                     value "user_id" (show d)
                     value "user_email" (a)
                     value "company_id" (show b)

instance KontraException UserShouldBeSelfOrCompanyAdmin

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

instance ToJSValue UserShouldBeDirectlyOrIndirectlyRelatedToDocument where
  toJSValue (UserShouldBeDirectlyOrIndirectlyRelatedToDocument d a g b) = runJSONGen $ do
                     value "message" ("User is not related to document" :: String)
                     value "user_id" (show d)
                     value "document_id" (show a)
                     value "document_title" (g)
                     value "user_email" (b)

instance KontraException UserShouldBeDirectlyOrIndirectlyRelatedToDocument

sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument :: (MonadState v m, SqlWhere v)
                                                    => UserID -> m ()
sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid =
  sqlWhereEVVV (UserShouldBeDirectlyOrIndirectlyRelatedToDocument uid, "(SELECT signatory_links.document_id)",
               "(SELECT documents.title FROM documents WHERE documents.id = signatory_links.document_id)",
               "(SELECT email FROM users WHERE id = " <?> uid <> ")")
              ("same_company_users.id = " <?> uid)

data DocumentDoesNotExist = DocumentDoesNotExist DocumentID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentDoesNotExist where
  toJSValue (DocumentDoesNotExist d) = runJSONGen $ do
                     value "message" ("Document does not exists" :: String)
                     value "document_id" (show d)

instance KontraException DocumentDoesNotExist

sqlWhereDocumentIDIs :: (MonadState v m, SqlWhere v)
                     => DocumentID -> m ()
sqlWhereDocumentIDIs did =
  sqlWhereE (DocumentDoesNotExist did) ("documents.id = " <?> did)

data SignatoryLinkDoesNotExist = SignatoryLinkDoesNotExist SignatoryLinkID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryLinkDoesNotExist where
  toJSValue (SignatoryLinkDoesNotExist d) = runJSONGen $ do
                     value "message" ("Signatory link does not exists" :: String)
                     value "signatory_link_id" (show d)

instance KontraException SignatoryLinkDoesNotExist

sqlWhereSignatoryLinkIDIs :: (MonadState v m, SqlWhere v)
                          => SignatoryLinkID -> m ()
sqlWhereSignatoryLinkIDIs slid =
  sqlWhereE (SignatoryLinkDoesNotExist slid) ("signatory_links.id = " <?> slid)


data SignatoryHasNotYetSigned = SignatoryHasNotYetSigned
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasNotYetSigned where
  toJSValue (SignatoryHasNotYetSigned) = runJSONGen $ do
                     value "message" ("Not all signatories have signed" :: String)

instance KontraException SignatoryHasNotYetSigned

sqlWhereAllSignatoriesHaveSigned :: (MonadState v m, SqlWhere v)
                                 => m ()
sqlWhereAllSignatoriesHaveSigned = sqlWhereE SignatoryHasNotYetSigned $
  "NOT EXISTS (SELECT TRUE FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner AND signatory_links.sign_time IS NULL)"



data SignatoryIsNotPartner = SignatoryIsNotPartner
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryIsNotPartner where
  toJSValue (SignatoryIsNotPartner) = runJSONGen $ do
                     value "message" ("Signatory is not partner" :: String)

instance KontraException SignatoryIsNotPartner

sqlWhereSignatoryIsPartner :: (MonadState v m, SqlWhere v)
                                 => m ()
sqlWhereSignatoryIsPartner = sqlWhereE SignatoryIsNotPartner $
  "signatory_links.is_partner"


data SignatoryHasAlreadySigned = SignatoryHasAlreadySigned
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasAlreadySigned where
  toJSValue (SignatoryHasAlreadySigned) = runJSONGen $ do
                     value "message" ("Signatory has already signed" :: String)

instance KontraException SignatoryHasAlreadySigned

sqlWhereSignatoryHasNotSigned :: (MonadState v m, SqlWhere v)
                                 => m ()
sqlWhereSignatoryHasNotSigned = sqlWhereE SignatoryHasAlreadySigned $
  "signatory_links.sign_time IS NULL"


data SignatoryTokenDoesNotMatch = SignatoryTokenDoesNotMatch
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryTokenDoesNotMatch where
  toJSValue (SignatoryTokenDoesNotMatch) = runJSONGen $ do
                     value "message" ("Signatory token does not match" :: String)

instance KontraException SignatoryTokenDoesNotMatch

sqlWhereSignatoryLinkMagicHashIs :: (MonadState v m, SqlWhere v)
                                 => MagicHash -> m ()
sqlWhereSignatoryLinkMagicHashIs mh = sqlWhereE SignatoryTokenDoesNotMatch $
  "signatory_links.token = " <?> mh
