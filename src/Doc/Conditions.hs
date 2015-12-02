module Doc.Conditions where

import Control.Monad.State.Class
import Data.Int
import Data.Typeable
import Text.JSON.Gen

import Company.CompanyID
import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import KontraPrelude
import MagicHash
import MinutesTime
import User.UserID

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
sqlWhereDocumentIDIs did = do
  sqlWhereE (DocumentDoesNotExist did) ("documents.id = " <?> did)

sqlWhereDocumentIDForSignatoryIs :: (MonadState v m, SqlWhere v)
                     => DocumentID -> m ()
sqlWhereDocumentIDForSignatoryIs did = do
  sqlWhereE (DocumentDoesNotExist did) ("signatory_links.document_id = " <?> did)

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

data SignatoryHasAlreadyAuthenticatedToView = SignatoryHasAlreadyAuthenticatedToView
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasAlreadyAuthenticatedToView where
  toJSValue SignatoryHasAlreadyAuthenticatedToView = runJSONGen $ do
    value "messsage" ("Signatory has already authenticated to view" :: String)

instance KontraException SignatoryHasAlreadyAuthenticatedToView

sqlWhereSignatoryHasNotAuthenticatedToView :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryHasNotAuthenticatedToView = sqlWhereE (SignatoryHasAlreadyAuthenticatedToView)
  "signatory_links.id NOT IN (SELECT signatory_link_id FROM eid_authentications)"


data SignatoryHasAlreadySigned = SignatoryHasAlreadySigned
  { signatoryHasAlreadySignedTime :: UTCTime
  }
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasAlreadySigned where
  toJSValue (SignatoryHasAlreadySigned {..}) = runJSONGen $ do
    value "message" ("Signatory has already signed" :: String)
    value "sign_time" (show signatoryHasAlreadySignedTime)

instance KontraException SignatoryHasAlreadySigned

sqlWhereSignatoryHasNotSigned :: (MonadState v m, SqlWhere v)
                                 => m ()
sqlWhereSignatoryHasNotSigned = sqlWhereEV (SignatoryHasAlreadySigned,"signatory_links.sign_time") $
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

data DocumentObjectVersionDoesNotMatch = DocumentObjectVersionDoesNotMatch
    { documentOdocumentObjectVersionDocumentID :: DocumentID
    , documentObjectVersionShouldBe            :: Int64
    , documentObjectVersionIs                  :: Int64
    }
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentObjectVersionDoesNotMatch where
  toJSValue (DocumentObjectVersionDoesNotMatch {..}) = runJSONGen $ do
    value "message" ("Document object version does not match" :: String)
    value "expected" (show documentObjectVersionShouldBe)
    value "actual" (show documentObjectVersionIs)

instance KontraException DocumentObjectVersionDoesNotMatch

sqlWhereDocumentObjectVersionIs :: (MonadState v m, SqlWhere v)
                                 => Int64 -> m ()
sqlWhereDocumentObjectVersionIs object_version = sqlWhereEVV (\did actual -> DocumentObjectVersionDoesNotMatch did object_version actual, "documents.id", "documents.object_version") $
 ("documents.object_version = " <?> object_version)

data DocumentWasPurged = DocumentWasPurged DocumentID String UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentWasPurged where
  toJSValue (DocumentWasPurged did title time) = runJSONGen $ do
    value "message" ("Document was purged from the system and is no longer available" :: String)
    value "document_id" (show did)
    value "purged_time" (show time)
    value "title" title

instance KontraException DocumentWasPurged

sqlWhereDocumentWasNotPurged :: (MonadState v m, SqlWhere v)
                             => m ()
sqlWhereDocumentWasNotPurged = sqlWhereEVVV (DocumentWasPurged,
                                             "documents.id", "documents.title","documents.purged_time")
                               "documents.purged_time IS NULL"

------------------------------------------------------------
data DocumentIsDeleted = DocumentIsDeleted DocumentID String SignatoryLinkID UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsDeleted where
  toJSValue (DocumentIsDeleted did title slid time) = runJSONGen $ do
    value "message" ("Document is deleted" :: String)
    value "document_id" (show did)
    value "signatory_link_id" (show slid)
    value "deleted_time" (show time)
    value "title" title

instance KontraException DocumentIsDeleted

sqlWhereDocumentIsNotDeleted :: (MonadState v m, SqlWhere v)
                             => m ()
sqlWhereDocumentIsNotDeleted = sqlWhereEVVVV (DocumentIsDeleted,
                                             "signatory_links.document_id", "(SELECT title FROM documents WHERE documents.id = document_id)","signatory_links.id", "signatory_links.deleted")
                               "signatory_links.deleted IS NULL"

------------------------------------------------------------

data DocumentIsNotDeleted = DocumentIsNotDeleted DocumentID String SignatoryLinkID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsNotDeleted where
  toJSValue (DocumentIsNotDeleted did title slid) = runJSONGen $ do
    value "message" ("Document is not deleted" :: String)
    value "document_id" (show did)
    value "signatory_link_id" (show slid)
    value "title" title

instance KontraException DocumentIsNotDeleted

sqlWhereDocumentIsDeleted :: (MonadState v m, SqlWhere v)
                             => m ()
sqlWhereDocumentIsDeleted = sqlWhereEVVV (DocumentIsNotDeleted,
                                             "signatory_links.document_id", "(SELECT title FROM documents WHERE documents.id = document_id)","signatory_links.id")
                               "signatory_links.deleted IS NOT NULL"

------------------------------------------------------------
data DocumentIsReallyDeleted = DocumentIsReallyDeleted DocumentID String SignatoryLinkID UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsReallyDeleted where
  toJSValue (DocumentIsReallyDeleted did title slid time) = runJSONGen $ do
    value "message" ("Document is really deleted" :: String)
    value "document_id" (show did)
    value "signatory_link_id" (show slid)
    value "deleted_time" (show time)
    value "title" title

instance KontraException DocumentIsReallyDeleted

sqlWhereDocumentIsNotReallyDeleted :: (MonadState v m, SqlWhere v)
                             => m ()
sqlWhereDocumentIsNotReallyDeleted = sqlWhereEVVVV (DocumentIsReallyDeleted,
                                             "signatory_links.id", "(SELECT title FROM documents WHERE documents.id = document_id)","signatory_links.id", "signatory_links.really_deleted")
                               "signatory_links.really_deleted IS NULL"

------------------------------------------------------------

data SignatoryAuthenticationToSignDoesNotMatch = SignatoryAuthenticationToSignDoesNotMatch DocumentID SignatoryLinkID AuthenticationToSignMethod AuthenticationToSignMethod
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryAuthenticationToSignDoesNotMatch where
  toJSValue (SignatoryAuthenticationToSignDoesNotMatch did slid expected actual) = runJSONGen $ do
    value "message" ("Signatory authentication to sign method does not match" :: String)
    value "document_id" (show did)
    value "signatory_link_id" (show slid)
    value "expected" (show expected)
    value "actual" (show actual)

instance KontraException SignatoryAuthenticationToSignDoesNotMatch

sqlWhereSignatoryAuthenticationToSignMethodIs :: (MonadState v m, SqlWhere v)
                                        => AuthenticationToSignMethod -> m ()
sqlWhereSignatoryAuthenticationToSignMethodIs am =
  sqlWhereEVVV (\did slid amact -> SignatoryAuthenticationToSignDoesNotMatch did slid am amact,
                "signatory_links.document_id",
                "signatory_links.id",
                "signatory_links.authentication_to_sign_method")
                ("signatory_links.authentication_to_sign_method = " <?> am)
