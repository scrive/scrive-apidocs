module Doc.Model.Domain
  ( DocumentDomain(..)
  , documentDomainToSQL
  ) where

import DB

import User.UserID
import Doc.DocStateData
import Control.Monad.State.Class
import MagicHash
import Data.Typeable

-- | Document security domain.
--
-- This data type should be regarder our most precious and important
-- security measure against unarthorized document access. When using
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
  | DocumentsVisibleToUser UserID                -- ^ Documents that a user has possible access to
  | DocumentsVisibleViaAccessToken MagicHash     -- ^ Documents accessed using 'accesstoken' field from json
 deriving (Eq, Ord, Typeable, Show)
--
-- Document visibility rules:
--
-- A document is either signable or template.
--
-- Any of number points must be true, all of letter subpoints must be
-- true.
--
-- Visibility rules:
--
-- 1. User can see her own authored documents.
-- 2. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. was invited to sign (is_partner = TRUE)
--     d. sign order says it is ok to see
-- 3. User can see a document when:
--     a. document is signable
--     b. document is not in Preparation
--     c. was invited to view (is_partner = FALSE)
-- 4. User can see a document when:
--     a. document is template
--     b. author is in the same company
--     c. document has company sharing set
-- 5. User can see a document when:
--     a. user is company admin
--     b. there is another user in the same company that can see the document
--     c. document is not in preparation state
--
-- A really_deleted document is never shown.
--
-- To do anything with document a user has at least see it. Usually
-- more strict rules apply.
documentDomainToSQL :: (MonadState v m, SqlWhere v)
                    => DocumentDomain
                    -> m ()
documentDomainToSQL (DocumentsOfWholeUniverse) = do
  sqlWhere "TRUE"
documentDomainToSQL (DocumentsVisibleViaAccessToken token) = do
  sqlWhereEq "documents.token" token
documentDomainToSQL (DocumentsVisibleToUser uid) =
  sqlWhereAny $ do
    sqlWhereAll $ do           -- 1: see own documents
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhere "signatory_links.is_author"
    sqlWhereAll $ do           -- 2. see signables as partner
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhereNotEq "documents.status" Preparation
      sqlWhereEq "documents.type" $ Signable
      sqlWhere "signatory_links.is_partner"
      sqlWhereNotExists $ sqlSelect "signatory_links AS earlier_signatory_links" $ do
                            sqlWhere "signatory_links.document_id = earlier_signatory_links.document_id"
                            sqlWhere "earlier_signatory_links.is_partner"
                            sqlWhere "earlier_signatory_links.sign_time IS NULL"
                            sqlWhere "earlier_signatory_links.sign_order < signatory_links.sign_order"
    sqlWhereAll $ do           -- 3. see signables as viewer
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "users.id = same_company_users.id"
      sqlWhereNotEq "documents.status" Preparation
      sqlWhereEq "documents.type" $ Signable
      sqlWhere "NOT signatory_links.is_partner"
    sqlWhereAll $ do           -- 4. see shared templates
      sqlWhereEq "same_company_users.id" uid
      sqlWhereEq "documents.sharing" Shared
      sqlWhereEq "documents.type" $ Template
      sqlWhere "signatory_links.is_author"
    sqlWhereAll $ do           -- 5: see documents of subordinates
      sqlWhereEq "same_company_users.id" uid
      sqlWhere "same_company_users.is_company_admin"
      sqlWhere "signatory_links.is_author"
      sqlWhereNotEq "documents.status" Preparation
