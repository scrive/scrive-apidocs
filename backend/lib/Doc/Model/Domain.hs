module Doc.Model.Domain
  ( DocumentDomain(..)
  , documentDomainToSQL
  , documentDomainNeedsDistinct
  ) where

import Control.Monad.State.Class
import Data.Typeable
import qualified Data.Set as S

import DB
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.Model.Filter
import Folder.Types
import MagicHash
import User.UserID
import UserGroup.Types

-- | Document security domain.
--
-- This data type should be regarded our most precious and important
-- security measure against unauthorized document access. When using
-- `GetDocuments` or derivatives we should always specify on whose
-- behalf are we acting upon a document. If we have user id then using
-- `DocumentsVisibleToUser` ensures that we are touching only
-- documents that we are allowed to touch and nothing else.
--
-- I wonder if we should refactor `DocumentDomain` into specialized
-- functions like GetDocumentsVisibleToUser [DocumentFilter]
-- [DocumentOrderBy] Pagination. For now it is `GetDocuments` and
-- parameters.
data DocumentDomain
  = DocumentsOfWholeUniverse
  -- ^ All documents in the system. Only for admin view.
  | DocumentsVisibleViaAccessToken MagicHash
  -- ^ Documents accessed using 'accesstoken' field from json
  | DocumentsOfUserGroup UserGroupID
  -- ^ Documents created by a particular user group.
  | DocumentsVisibleToUser UserID
  -- ^ Documents that a user has possible access to
  | DocumentsByFolderOnly FolderID
  -- ^ List documents in folder for which user has read access and have not been
  -- deleted by their author.
  | DocumentsUserHasAnyLinkTo UserID
  -- ^ Documents that the given user is linked to
  | DocumentsVisibleToSigningPartyOrByFolders UserID [FolderID] [FolderID] [FolderID]
  -- ^ Documents that a user has access to by means of some read access to a folder
  --   shared_template_fids
  --   started_documents_fids
  --   full_read_fids (usually authors Folders)
 deriving (Eq, Ord, Typeable, Show)

-- | Domain needs DISTINCT when there could be duplicate document ids, i.e.
-- there is no UNION and we join with signatory_links table.
documentDomainNeedsDistinct :: DocumentDomain -> Bool
documentDomainNeedsDistinct = \case
  DocumentsOfWholeUniverse{}       -> False
  DocumentsVisibleViaAccessToken{} -> False
  DocumentsOfUserGroup{}           -> True
  DocumentsVisibleToUser{}         -> True
  DocumentsByFolderOnly{}          -> False
  DocumentsUserHasAnyLinkTo{}      -> True
  DocumentsVisibleToSigningPartyOrByFolders{} -> False

--
-- Document visibility rules:
--
-- A document is either signable or template.
--
-- Any of number points must be true, all of letter subpoints must be
-- true.
--
-- Visibility rules for a specific user:
--
-- 1. User can see her own authored documents.
-- 2. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. user was invited to sign or approve (based on signatory_role field value)
--     d. its sign order says it is ok to see
-- 3. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. user was invited to view (based on signatory_role field value)
-- 4. User can see a document when:
--     a. document is template
--     b. its author is in the same company
--     c. document has company sharing set
-- 5. User can see a document when:
--     a. user is company admin
--     b. there is another user in the same company that created the document
--     c. document is not in preparation state
--
-- To do anything with document a user has at least see it. Usually
-- more strict rules apply.

documentDomainToSQL
  :: (MonadState v m, SqlFrom v, SqlWhere v)
  => DocumentDomain
  -> [(DocumentFilterMap, m ())]
documentDomainToSQL DocumentsOfWholeUniverse = pure . (legacyFilterMap, ) $ do
  sqlWhereDocumentWasNotPurged
  sqlWhereAnySignatoryLinkNotReallyDeleted

documentDomainToSQL (DocumentsVisibleViaAccessToken token) =
  pure . (legacyFilterMap, ) $ do
    sqlWhereDocumentWasNotPurged
    sqlWhereAnySignatoryLinkNotReallyDeleted
    sqlWhereEq "documents.token" token

documentDomainToSQL (DocumentsOfUserGroup ugid) = pure . (legacyFilterMap, ) $ do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlJoinOn "users"           "signatory_links.user_id = users.id"
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeleted
  sqlWhere "documents.author_id = signatory_links.id"
  sqlWhereEq "users.user_group_id" ugid

documentDomainToSQL (DocumentsVisibleToUser uid) = pure . (legacyFilterMap, ) $ do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlJoinOn "users"           "signatory_links.user_id = users.id"
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeleted
  sqlWhere
    $   "users.user_group_id = (SELECT u.user_group_id FROM users u WHERE u.id ="
    <?> uid
    <>  ")"
  sqlWhereAny
    [ userIsAuthor
    , userIsSignatoryOrApproverAndHasAppropriateSignOrder uid
    , userIsViewer uid
    , isCompanySharedTemplate
    , isCompanyDocumentIfAdmin
    ]
  where
    userIsAuthor = do
      sqlWhereEq "users.id" uid
      sqlWhere "documents.author_id = signatory_links.id"

    isCompanyDocumentIfAdmin = do
      sqlWhere $ "(SELECT u.is_company_admin FROM users u WHERE u.id =" <?> uid <> ")"
      sqlWhere "documents.author_id = signatory_links.id"
      sqlWhereNotEq "documents.status" Preparation

    isCompanySharedTemplate = do
      sqlWhere "documents.author_id = signatory_links.id"
      sqlWhereEq "documents.type"    Template
      sqlWhereEq "documents.sharing" Shared

documentDomainToSQL (DocumentsByFolderOnly fdrid) = pure . (folderFilterMap, ) $ do
  sqlWhereEq "folder_id" fdrid
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeletedByAuthor

documentDomainToSQL (DocumentsUserHasAnyLinkTo uid) = pure . (legacyFilterMap, ) $ do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlWhereEq "user_id" uid
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeleted

{-
  In the future we should handle document access mostly or exclusively through
  the folder and access control systems. The below mixes the folder approach and
  the legacy approach since we're in a transition phase. Ideally,
  `DocumentsVisibleToSigningPartyOrByFolders` should retrieve the same set of
  documents (for now at least) as `DocumentsVisibleToUser`, if the Folders
  generated by default were not changed.

  The old list calls return all documents, which the user can act on as
  SigningParty or they are author or authors admin.

  The new list calls return all documents, which the user can act on as
  SigningParty or the document is in one of the Folders (which correspond to the
  old behaviour for default Folder setup).

  `UserID` is included in the data constructor because of the SigningParty based
  access.
-}
documentDomainToSQL (DocumentsVisibleToSigningPartyOrByFolders uid shared_fids started_fids author_fids)
  = [ (legacyFilterMap, documentsVisibleToSigningParty)
    , (folderFilterMap, documentsVisibleByFolders)
    ]
  where
    documentsVisibleToSigningParty = do
      sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
      sqlJoinOn "users"           "signatory_links.user_id = users.id"
      sqlWhereEq "signatory_links.user_id" uid
      sqlWhereDocumentWasNotPurged
      sqlWhereDocumentIsNotReallyDeleted
      sqlWhereAny
        [userIsSignatoryOrApproverAndHasAppropriateSignOrder uid, userIsViewer uid]

    documentsVisibleByFolders = do
      sqlJoinOn "folders" "folders.id = documents.folder_id"
      sqlWhereIn "folders.id"
                 (S.toList . S.fromList $ author_fids ++ shared_fids ++ started_fids)
      sqlWhereDocumentWasNotPurged
      sqlWhereDocumentIsNotReallyDeletedByAuthor
      sqlWhereAny
        [ documentIsInFolder author_fids
        , documentIsSharedTemplateAndIsInFolder shared_fids
        , documentIsStartedAndIsInFolder started_fids
        ]

    documentIsInFolder fids = do
      sqlWhereIn "folders.id" fids

    documentIsStartedAndIsInFolder fids = do
      sqlWhereIn "folders.id" fids
      sqlWhereNotEq "documents.status" Preparation

    documentIsSharedTemplateAndIsInFolder fids = do
      sqlWhereIn "folders.id" fids
      sqlWhereEq "documents.type"    Template
      sqlWhereEq "documents.sharing" Shared

-- helpers

userIsViewer :: (MonadState v m, SqlWhere v) => UserID -> m ()
userIsViewer uid = do
  sqlWhereEq "users.id"       uid
  sqlWhereEq "documents.type" Signable
  sqlWhereNotEq "documents.status" Preparation
  sqlWhereSignatoryRoleIsViewer

userIsSignatoryOrApproverAndHasAppropriateSignOrder
  :: (MonadState v m, SqlWhere v) => UserID -> m ()
userIsSignatoryOrApproverAndHasAppropriateSignOrder uid = do
  sqlWhereEq "users.id"       uid
  sqlWhereEq "documents.type" Signable
  sqlWhereIn "documents.status" documentStatusesAccessibleBySignatories
  sqlWhereSignatoryRoleIsSigningPartyOrApprover
  sqlWhereNotExists . sqlSelect "signatory_links AS osl" $ do
    sqlWhere "signatory_links.document_id = osl.document_id"
    sqlWhere "osl.sign_time IS NULL"
    sqlWhere
      .   parenthesize
      $   "osl.signatory_role ="
      <?> SignatoryRoleSigningParty
      <+> "OR osl.signatory_role ="
      <?> SignatoryRoleApprover
    sqlWhere "osl.sign_order < signatory_links.sign_order"
