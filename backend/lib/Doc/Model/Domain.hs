module Doc.Model.Domain
  ( DocumentDomain(..)
  , documentDomainToSQL
  ) where

import Control.Monad.State.Class
import Data.Typeable

import DB
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
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
  = DocumentsOfWholeUniverse                     -- ^ All documents in the system. Only for admin view.
  | DocumentsVisibleViaAccessToken MagicHash     -- ^ Documents accessed using 'accesstoken' field from json
  | DocumentsOfUserGroup UserGroupID             -- ^ Documents created by a particular user group.
  | DocumentsVisibleToUser UserID                -- ^ Documents that a user has possible access to
  | DocumentsByFolderOnly FolderID               -- ^ List documents in folder for which user has read access
 deriving (Eq, Ord, Typeable, Show)
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

documentDomainToSQL :: (MonadState v m, SqlFrom v, SqlWhere v)
                     => DocumentDomain
                     -> m ()
documentDomainToSQL DocumentsOfWholeUniverse = do
  sqlWhereDocumentWasNotPurged

documentDomainToSQL (DocumentsVisibleViaAccessToken token) = do
  sqlWhereDocumentWasNotPurged
  sqlWhereEq "documents.token" token

documentDomainToSQL (DocumentsOfUserGroup ugid) = do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlJoinOn "users" "signatory_links.user_id = users.id"
  sqlWhereDocumentWasNotPurged
  sqlWhere "documents.author_id = signatory_links.id"
  sqlWhereEq "users.user_group_id" ugid

documentDomainToSQL (DocumentsVisibleToUser uid) = do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlJoinOn "users" "signatory_links.user_id = users.id"
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeleted
  sqlWhere $ "users.user_group_id = (SELECT u.user_group_id FROM users u WHERE u.id =" <?> uid <> ")"
  sqlWhereAny [
      userIsAuthor
    , userIsSignatoryOrApproverAndHasAppropriateSignOrder
    , userIsViewer
    , isCompanySharedTemplate
    , isCompanyDocumentIfAdmin
    ]
  where
    userIsAuthor = do
      sqlWhereEq "users.id" uid
      sqlWhere "documents.author_id = signatory_links.id"

    userIsSignatoryOrApproverAndHasAppropriateSignOrder = do
      sqlWhereEq "users.id" uid
      sqlWhereEq "documents.type" Signable
      sqlWhereIn "documents.status" documentStatusesAccessibleBySignatories
      sqlWhereSignatoryRoleIsSigningPartyOrApprover
      sqlWhereNotExists . sqlSelect "signatory_links AS osl" $ do
        sqlWhere "signatory_links.document_id = osl.document_id"
        sqlWhere "osl.sign_time IS NULL"
        sqlWhere . parenthesize $
          "osl.signatory_role ="    <?> SignatoryRoleSigningParty <+>
          "OR osl.signatory_role =" <?> SignatoryRoleApprover
        sqlWhere "osl.sign_order < signatory_links.sign_order"

    userIsViewer = do
      sqlWhereEq "users.id" uid
      sqlWhereEq "documents.type" Signable
      sqlWhereNotEq "documents.status" Preparation
      sqlWhereSignatoryRoleIsViewer

    isCompanySharedTemplate = do
      sqlWhere "documents.author_id = signatory_links.id"
      sqlWhereEq "documents.type" Template
      sqlWhereEq "documents.sharing" Shared

    isCompanyDocumentIfAdmin = do
      sqlWhere $ "(SELECT u.is_company_admin FROM users u WHERE u.id =" <?> uid <> ")"
      sqlWhere "documents.author_id = signatory_links.id"
      sqlWhereNotEq "documents.status" Preparation

documentDomainToSQL (DocumentsByFolderOnly fdrid) = do
  sqlJoinOn "signatory_links" "documents.id = signatory_links.document_id"
  sqlWhereEq "folder_id" fdrid
  sqlWhereDocumentWasNotPurged
  sqlWhereDocumentIsNotReallyDeleted
