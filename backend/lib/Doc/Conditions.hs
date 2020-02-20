module Doc.Conditions where

import Control.Monad.State.Class
import Data.Int
import Data.Typeable
import Text.JSON.Gen

import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryConsentQuestionID
import Doc.SignatoryLinkID
import MagicHash
import MinutesTime
import User.UserID
import UserGroup.Types

-- This is the part where we define all that could possibly go wrong
-- with a document.


data DocumentTypeShouldBe = DocumentTypeShouldBe
  { documentTypeShouldBe :: DocumentType
  , documentTypeIs       :: DocumentType
  } deriving (Eq, Ord, Typeable)

instance Show DocumentTypeShouldBe where
  show (DocumentTypeShouldBe a b) =
    "DocumentTypeShouldBe { documentTypeShouldBe = "
      ++ x a
      ++ ", documentTypeIs = "
      ++ x b
      ++ " }"
    where
      x (Signable{}) = "Signable"
      x (Template{}) = "Template"

instance ToJSValue DocumentTypeShouldBe where
  toJSValue (DocumentTypeShouldBe a b) = runJSONGen $ do
    value "message"  ("Document has incorrect type" :: String)
    value "expected" (x a)
    value "actual"   (x b)
    where
      x (Signable{}) = ("Signable" :: String)
      x (Template{}) = "Template"


instance DBExtraException DocumentTypeShouldBe

sqlWhereDocumentTypeIs :: (MonadState v m, SqlWhere v) => DocumentType -> m ()
sqlWhereDocumentTypeIs xtype =
  sqlWhereEV (DocumentTypeShouldBe xtype, "documents.type") $ "documents.type =" <?> xtype

data DocumentStatusShouldBe = DocumentStatusShouldBe
  { documentStatusShouldBeDocumentID :: DocumentID
  , documentStatusShouldBe           :: [DocumentStatus]
  , documentStatusIs                 :: DocumentStatus
  } deriving (Eq, Ord, Typeable)

instance Show DocumentStatusShouldBe where
  show (DocumentStatusShouldBe d a b) =
    "DocumentStatusShouldBe { documentStatusShouldBeDocumentID = "
      ++ show d
      ++ ", documentStatusShouldBe = ["
      ++ intercalate "," (map x a)
      ++ "], documentStatusIs = "
      ++ x b
      ++ " }"
    where
      x (DocumentError{}) = "DocumentError {}"
      x v                 = show v

instance ToJSValue DocumentStatusShouldBe where
  toJSValue (DocumentStatusShouldBe d a b) = runJSONGen $ do
    value "message"     ("Document status is incorrect" :: String)
    value "document_id" (show d)
    value "expected"    (map x a)
    value "actual"      (x b)
    where
      x (DocumentError{}) = "DocumentError {}"
      x v                 = show v

instance DBExtraException DocumentStatusShouldBe

sqlWhereDocumentStatusIsOneOf :: (MonadState v m, SqlWhere v) => [DocumentStatus] -> m ()
sqlWhereDocumentStatusIsOneOf [] = sqlWhereEVV
  (\d -> DocumentStatusShouldBe d [], "documents.id", "documents.status")
  "FALSE"
sqlWhereDocumentStatusIsOneOf [s] = sqlWhereEVV
  (\d -> DocumentStatusShouldBe d [s], "documents.id", "documents.status")
  ("documents.status =" <?> s)
sqlWhereDocumentStatusIsOneOf sx = sqlWhereEVV
  (\d -> DocumentStatusShouldBe d sx, "documents.id", "documents.status")
  ("documents.status IN" <+> parenthesize (sqlConcatComma (map sqlParam sx)))

sqlWhereDocumentStatusIs :: (MonadState v m, SqlWhere v) => DocumentStatus -> m ()
sqlWhereDocumentStatusIs status = sqlWhereDocumentStatusIsOneOf [status]


data UserShouldBeSelfOrCompanyAdmin = UserShouldBeSelfOrCompanyAdmin
  { userShouldBeSelfOrCompanyAdminUserID    :: UserID
  , userShouldBeSelfOrCompanyAdminUserEmail :: String
  , userShouldBeSelfOrCompanyAdminUserGroupID :: UserGroupID
  } deriving (Eq, Ord, Typeable, Show)

instance ToJSValue UserShouldBeSelfOrCompanyAdmin where
  toJSValue (UserShouldBeSelfOrCompanyAdmin d a b) = runJSONGen $ do
    value "message"    ("User is not company admin" :: String)
    value "user_id"    (show d)
    value "user_email" (a)
    value "company_id" (show b)

instance DBExtraException UserShouldBeSelfOrCompanyAdmin

sqlWhereUserIsSelfOrCompanyAdmin :: (MonadState v m, SqlWhere v) => m ()
sqlWhereUserIsSelfOrCompanyAdmin = sqlWhereEVVV
  ( UserShouldBeSelfOrCompanyAdmin
  , "same_usergroup_users.id"
  , "same_usergroup_users.email"
  , "same_usergroup_users.user_group_id"
  )
  ("(users.id = same_usergroup_users.id" <+> "OR same_usergroup_users.is_company_admin)")

data UserShouldBeDirectlyOrIndirectlyRelatedToDocument =
  UserShouldBeDirectlyOrIndirectlyRelatedToDocument
  { userShouldBeDirectlyOrIndirectlyRelatedToDocumentUserID        :: UserID
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentDocumentID    :: DocumentID
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentDocumentTitle :: String
  , userShouldBeDirectlyOrIndirectlyRelatedToDocumentUserEmail     :: String
  } deriving (Eq, Ord, Typeable, Show)

instance ToJSValue UserShouldBeDirectlyOrIndirectlyRelatedToDocument where
  toJSValue (UserShouldBeDirectlyOrIndirectlyRelatedToDocument d a g b) = runJSONGen $ do
    value "message"        ("User is not related to document" :: String)
    value "user_id"        (show d)
    value "document_id"    (show a)
    value "document_title" (g)
    value "user_email"     (b)

instance DBExtraException UserShouldBeDirectlyOrIndirectlyRelatedToDocument

sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument
  :: (MonadState v m, SqlWhere v) => UserID -> m ()
sqlWhereUserIsDirectlyOrIndirectlyRelatedToDocument uid = sqlWhereEVVV
  ( UserShouldBeDirectlyOrIndirectlyRelatedToDocument uid
  , "(SELECT signatory_links.document_id)"
  , "(SELECT documents.title FROM documents"
    <+> "WHERE documents.id = signatory_links.document_id)"
  , "(SELECT email FROM users WHERE id =" <?> uid <> ")"
  )
  ("same_usergroup_users.id =" <?> uid)

data DocumentDoesNotExist = DocumentDoesNotExist DocumentID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentDoesNotExist where
  toJSValue (DocumentDoesNotExist d) = runJSONGen $ do
    value "message"     ("Document does not exists" :: String)
    value "document_id" (show d)

instance DBExtraException DocumentDoesNotExist

sqlWhereDocumentIDIs :: (MonadState v m, SqlWhere v) => DocumentID -> m ()
sqlWhereDocumentIDIs did = do
  sqlWhereE (DocumentDoesNotExist did) ("documents.id =" <?> did)

data ShortDocumentIDHasNoMatch = ShortDocumentIDHasNoMatch DocumentID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue ShortDocumentIDHasNoMatch where
  toJSValue (ShortDocumentIDHasNoMatch d) = runJSONGen $ do
    value "message"           ("No match for short document ID" :: String)
    value "short_document_id" (show d)

instance DBExtraException ShortDocumentIDHasNoMatch

sqlWhereShortDocumentIDIs
  :: (MonadState v m, SqlWhere v, SqlOrderBy v, SqlOffsetLimit v)
  => UTCTime
  -> DocumentID
  -> m ()
sqlWhereShortDocumentIDIs now shortDid = do
  sqlWhereE (ShortDocumentIDHasNoMatch shortDid) ("documents.id % 1000000 =" <?> shortDid)
  sqlWhereE (ShortDocumentIDHasNoMatch shortDid)
    $ (   "documents.id >="
      <+> "(SELECT COALESCE(max(id),0) FROM documents WHERE mtime <"
      <?> now
      <+> "- interval '72 hour')"
      <+> "and documents.mtime >"
      <?> now
      <+> "- interval '72 hour'"
      )
  sqlOrderBy "documents.id DESC"
  sqlLimit 1

sqlWhereDocumentIDForSignatoryIs :: (MonadState v m, SqlWhere v) => DocumentID -> m ()
sqlWhereDocumentIDForSignatoryIs did =
  sqlWhereE (DocumentDoesNotExist did) ("signatory_links.document_id =" <?> did)

data SignatoryLinkDoesNotExist = SignatoryLinkDoesNotExist SignatoryLinkID
  deriving (Eq, Ord, Show, Typeable)


instance ToJSValue SignatoryLinkDoesNotExist where
  toJSValue (SignatoryLinkDoesNotExist d) = runJSONGen $ do
    value "message"           ("Signatory link does not exists" :: String)
    value "signatory_link_id" (show d)

instance DBExtraException SignatoryLinkDoesNotExist

sqlWhereSignatoryLinkIDIs :: (MonadState v m, SqlWhere v) => SignatoryLinkID -> m ()
sqlWhereSignatoryLinkIDIs slid =
  sqlWhereE (SignatoryLinkDoesNotExist slid) ("signatory_links.id =" <?> slid)

data SignatoryLinkIsForwarded = SignatoryLinkIsForwarded
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryLinkIsForwarded where
  toJSValue (SignatoryLinkIsForwarded) = runJSONGen $ do
    value "message" ("Signatory link is forwarded" :: String)

instance DBExtraException SignatoryLinkIsForwarded

sqlWhereSignatoryLinkIsNotForwaded :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryLinkIsNotForwaded = sqlWhereE
  (SignatoryLinkIsForwarded)
  (   "signatory_links.signatory_role NOT IN "
  <+> parenthesize (sqlConcatComma (map sqlParam forwardedRoles))
  )

forwardedRoles :: [SignatoryRole]
forwardedRoles = [SignatoryRoleForwardedSigningParty, SignatoryRoleForwardedApprover]

data SigningPartyHasNotYetSignedOrApproved = SigningPartyHasNotYetSignedOrApproved
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SigningPartyHasNotYetSignedOrApproved where
  toJSValue (SigningPartyHasNotYetSignedOrApproved) = runJSONGen $ do
    value "message" ("Not all signing parties have signed or approved" :: String)

instance DBExtraException SigningPartyHasNotYetSignedOrApproved

sqlWhereAllSigningPartiesHaveSignedOrApproved :: (MonadState v m, SqlWhere v) => m ()
sqlWhereAllSigningPartiesHaveSignedOrApproved =
  sqlWhereE SigningPartyHasNotYetSignedOrApproved
    $   "NOT EXISTS"
    <+> "(SELECT TRUE FROM signatory_links"
    <+> "WHERE signatory_links.document_id = documents.id"
    <+> "AND"
    <+> parenthesize
          (   "signatory_links.signatory_role ="
          <?> SignatoryRoleSigningParty
          <+> "OR"
          <+> "signatory_links.signatory_role ="
          <?> SignatoryRoleApprover
          )
    <+> "AND signatory_links.sign_time IS NULL)"



data SignatoryRoleIsNotSigningParty = SignatoryRoleIsNotSigningParty
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryRoleIsNotSigningParty where
  toJSValue (SignatoryRoleIsNotSigningParty) = runJSONGen $ do
    value "message" ("Signatory's role is not signing party" :: String)

instance DBExtraException SignatoryRoleIsNotSigningParty

sqlWhereSignatoryRoleIsSigningParty :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryRoleIsSigningParty =
  sqlWhereE SignatoryRoleIsNotSigningParty
    $   "signatory_links.signatory_role ="
    <?> SignatoryRoleSigningParty

data SignatoryRoleIsNotSigningPartyOrApprover =
  SignatoryRoleIsNotSigningPartyOrApprover
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryRoleIsNotSigningPartyOrApprover where
  toJSValue (SignatoryRoleIsNotSigningPartyOrApprover) = runJSONGen $ do
    value "message" ("Signatory's role is not signing party or approver" :: String)

instance DBExtraException SignatoryRoleIsNotSigningPartyOrApprover

sqlWhereSignatoryRoleIsSigningPartyOrApprover :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryRoleIsSigningPartyOrApprover =
  sqlWhereE SignatoryRoleIsNotSigningPartyOrApprover
    .   parenthesize
    $   "signatory_links.signatory_role ="
    <?> SignatoryRoleSigningParty
    <+> "OR signatory_links.signatory_role ="
    <?> SignatoryRoleApprover

data SignatoryRoleMustNotBeSigningParty = SignatoryRoleMustNotBeSigningParty
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryRoleMustNotBeSigningParty where
  toJSValue (SignatoryRoleMustNotBeSigningParty) = runJSONGen $ do
    value "message" ("Signatory's role must not be signing party" :: String)

instance DBExtraException SignatoryRoleMustNotBeSigningParty

sqlWhereSignatoryRoleIsNotSigningParty :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryRoleIsNotSigningParty =
  sqlWhereE SignatoryRoleMustNotBeSigningParty
    $   "signatory_links.signatory_role !="
    <?> SignatoryRoleSigningParty

data SignatoryRoleIsNotApprover = SignatoryRoleIsNotApprover
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryRoleIsNotApprover where
  toJSValue (SignatoryRoleIsNotApprover) = runJSONGen $ do
    value "message" ("Signatory's role is not approver" :: String)

instance DBExtraException SignatoryRoleIsNotApprover

sqlWhereSignatoryRoleIsApprover :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryRoleIsApprover =
  sqlWhereE SignatoryRoleIsNotApprover
    $   "signatory_links.signatory_role ="
    <?> SignatoryRoleApprover


data SignatoryRoleIsNotViewer = SignatoryRoleIsNotViewer
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryRoleIsNotViewer where
  toJSValue (SignatoryRoleIsNotViewer) = runJSONGen $ do
    value "message" ("Signatory's role is not viewer" :: String)

instance DBExtraException SignatoryRoleIsNotViewer

sqlWhereSignatoryRoleIsViewer :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSignatoryRoleIsViewer =
  sqlWhereE SignatoryRoleIsNotViewer
    $   "signatory_links.signatory_role ="
    <?> SignatoryRoleViewer



data SignatoryHasAlreadyAuthenticatedToView =
  SignatoryHasAlreadyAuthenticatedToView
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasAlreadyAuthenticatedToView where
  toJSValue SignatoryHasAlreadyAuthenticatedToView = runJSONGen $ do
    value "messsage" ("Signatory has already authenticated to view" :: String)

instance DBExtraException SignatoryHasAlreadyAuthenticatedToView

sqlWhereSigningPartyHasNotAuthenticatedToView :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSigningPartyHasNotAuthenticatedToView = sqlWhereE
  SignatoryHasAlreadyAuthenticatedToView
  ("signatory_links.id NOT IN" <+> "(SELECT signatory_link_id FROM eid_authentications)")

data SignatoryHasAlreadySigned = SignatoryHasAlreadySigned
  { signatoryHasAlreadySignedTime :: UTCTime }
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryHasAlreadySigned where
  toJSValue (SignatoryHasAlreadySigned {..}) = runJSONGen $ do
    value "message"   ("Signatory has already signed" :: String)
    value "sign_time" (show signatoryHasAlreadySignedTime)

instance DBExtraException SignatoryHasAlreadySigned

sqlWhereSigningPartyHasNotSignedOrApproved :: (MonadState v m, SqlWhere v) => m ()
sqlWhereSigningPartyHasNotSignedOrApproved = sqlWhereEV
  (SignatoryHasAlreadySigned, "signatory_links.sign_time")
  "signatory_links.sign_time IS NULL"


data SignatoryTokenDoesNotMatch = SignatoryTokenDoesNotMatch
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryTokenDoesNotMatch where
  toJSValue (SignatoryTokenDoesNotMatch) = runJSONGen $ do
    value "message" ("Signatory token does not match" :: String)

instance DBExtraException SignatoryTokenDoesNotMatch

-- | Check that the signatory link has a matching magic hash. This does NOT
-- mean that the magic hash is valid. In order to avoid duplicating logic, a
-- further check needs to be done with `isValidSignatoryMagicHash`.
--
-- This check is still useful to avoid fetching records from DB when the magic
-- hash is obviously wrong.
sqlWhereSomeSignatoryAccessTokenHasMagicHash
  :: (MonadState v m, SqlWhere v) => MagicHash -> m ()
sqlWhereSomeSignatoryAccessTokenHasMagicHash mh =
  sqlWhereExists . sqlSelect "signatory_access_tokens" $ do
    sqlWhere "signatory_access_tokens.signatory_link_id = signatory_links.id"
    sqlWhereEq "signatory_access_tokens.hash" mh


data DocumentObjectVersionDoesNotMatch = DocumentObjectVersionDoesNotMatch
    { documentOdocumentObjectVersionDocumentID :: DocumentID
    , documentObjectVersionShouldBe            :: Int64
    , documentObjectVersionIs                  :: Int64
    }
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentObjectVersionDoesNotMatch where
  toJSValue (DocumentObjectVersionDoesNotMatch {..}) = runJSONGen $ do
    value "message"  ("Document object version does not match" :: String)
    value "expected" (show documentObjectVersionShouldBe)
    value "actual"   (show documentObjectVersionIs)

instance DBExtraException DocumentObjectVersionDoesNotMatch

sqlWhereDocumentObjectVersionIs :: (MonadState v m, SqlWhere v) => Int64 -> m ()
sqlWhereDocumentObjectVersionIs object_version =
  sqlWhereEVV
      ( \did actual -> DocumentObjectVersionDoesNotMatch did object_version actual
      , "documents.id"
      , "documents.object_version"
      )
    $ ("documents.object_version =" <?> object_version)

data DocumentWasPurged = DocumentWasPurged DocumentID String UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentWasPurged where
  toJSValue (DocumentWasPurged did title time) = runJSONGen $ do
    value "message"
          ("Document was purged from the system and is no longer available" :: String)
    value "document_id" (show did)
    value "purged_time" (show time)
    value "title"       (title)

instance DBExtraException DocumentWasPurged

sqlWhereDocumentWasNotPurged :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentWasNotPurged = sqlWhereEVVV
  (DocumentWasPurged, "documents.id", "documents.title", "documents.purged_time")
  "documents.purged_time IS NULL"

------------------------------------------------------------
data DocumentIsDeleted =
  DocumentIsDeleted DocumentID String SignatoryLinkID UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsDeleted where
  toJSValue (DocumentIsDeleted did title slid time) = runJSONGen $ do
    value "message"           ("Document is deleted" :: String)
    value "document_id"       (show did)
    value "signatory_link_id" (show slid)
    value "deleted_time"      (show time)
    value "title"             (title)

instance DBExtraException DocumentIsDeleted

sqlWhereDocumentIsNotDeleted :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentIsNotDeleted = sqlWhereEVVVV
  ( DocumentIsDeleted
  , "signatory_links.document_id"
  , "(SELECT title FROM documents WHERE documents.id = document_id)"
  , "signatory_links.id"
  , "signatory_links.deleted"
  )
  "signatory_links.deleted IS NULL"

------------------------------------------------------------

data DocumentIsNotDeleted =
  DocumentIsNotDeleted DocumentID String SignatoryLinkID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsNotDeleted where
  toJSValue (DocumentIsNotDeleted did title slid) = runJSONGen $ do
    value "message"           ("Document is not deleted" :: String)
    value "document_id"       (show did)
    value "signatory_link_id" (show slid)
    value "title"             (title)

instance DBExtraException DocumentIsNotDeleted

sqlWhereDocumentIsDeleted :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentIsDeleted = sqlWhereEVVV
  ( DocumentIsNotDeleted
  , "signatory_links.document_id"
  , "(SELECT title FROM documents WHERE documents.id = document_id)"
  , "signatory_links.id"
  )
  "signatory_links.deleted IS NOT NULL"

------------------------------------------------------------

sqlWhereDocumentIsNotDeletedByAuthor :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentIsNotDeletedByAuthor = sqlWhere "documents.author_deleted IS NULL"

------------------------------------------------------------

data DocumentIsReallyDeleted =
  DocumentIsReallyDeleted DocumentID String SignatoryLinkID UTCTime
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue DocumentIsReallyDeleted where
  toJSValue (DocumentIsReallyDeleted did title slid time) = runJSONGen $ do
    value "message"           ("Document is really deleted" :: String)
    value "document_id"       (show did)
    value "signatory_link_id" (show slid)
    value "deleted_time"      (show time)
    value "title"             (title)

instance DBExtraException DocumentIsReallyDeleted

sqlWhereDocumentIsNotReallyDeleted :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentIsNotReallyDeleted = sqlWhereEVVVV
  ( DocumentIsReallyDeleted
  , "signatory_links.id"
  , "(SELECT title FROM documents WHERE documents.id = document_id)"
  , "signatory_links.id"
  , "signatory_links.really_deleted"
  )
  "signatory_links.really_deleted IS NULL"

------------------------------------------------------------

sqlWhereDocumentIsNotReallyDeletedByAuthor :: (MonadState v m, SqlWhere v) => m ()
sqlWhereDocumentIsNotReallyDeletedByAuthor =
  sqlWhere "documents.author_really_deleted IS NULL"

------------------------------------------------------------

data SignatoryAuthenticationToSignDoesNotMatch =
  SignatoryAuthenticationToSignDoesNotMatch
  DocumentID SignatoryLinkID
  AuthenticationToSignMethod AuthenticationToSignMethod
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryAuthenticationToSignDoesNotMatch where
  toJSValue (SignatoryAuthenticationToSignDoesNotMatch did slid expected actual) =
    runJSONGen $ do
      value "message" ("Signatory authentication to sign method does not match" :: String)
      value "document_id"       (show did)
      value "signatory_link_id" (show slid)
      value "expected"          (show expected)
      value "actual"            (show actual)

instance DBExtraException SignatoryAuthenticationToSignDoesNotMatch

sqlWhereSignatoryAuthenticationToSignMethodIs
  :: (MonadState v m, SqlWhere v) => AuthenticationToSignMethod -> m ()
sqlWhereSignatoryAuthenticationToSignMethodIs am = sqlWhereEVVV
  ( \did slid amact -> SignatoryAuthenticationToSignDoesNotMatch did slid am amact
  , "signatory_links.document_id"
  , "signatory_links.id"
  , "signatory_links.authentication_to_sign_method"
  )
  ("signatory_links.authentication_to_sign_method =" <?> am)

------------------------------------------------------------

data SignatoryConsentQuestionDoesNotExist
  = SignatoryConsentQuestionDoesNotExist SignatoryConsentQuestionID
  deriving (Eq, Ord, Show, Typeable)

instance ToJSValue SignatoryConsentQuestionDoesNotExist where
  toJSValue (SignatoryConsentQuestionDoesNotExist d) = runJSONGen $ do
    value "message" ("Signatory consent question does not exist" :: String)
    value "signatory_consent_question_id" (show d)

instance DBExtraException SignatoryConsentQuestionDoesNotExist

sqlWhereSignatoryConsentQuestionIDIs
  :: (MonadState v m, SqlWhere v) => SignatoryConsentQuestionID -> m ()
sqlWhereSignatoryConsentQuestionIDIs scqid = sqlWhereE
  (SignatoryConsentQuestionDoesNotExist scqid)
  ("signatory_link_consent_questions.id =" <?> scqid)

------------------------------------------------------------

sqlWhereAnySignatoryLinkNotReallyDeleted :: (MonadState v m, SqlWhere v) => m ()
sqlWhereAnySignatoryLinkNotReallyDeleted = do
  sqlWhere . toSQLCommand $ sqlSelect "signatory_links" $ do
    sqlResult "bool_or(signatory_links.really_deleted IS NULL)"
    sqlWhere "signatory_links.document_id = documents.id"
