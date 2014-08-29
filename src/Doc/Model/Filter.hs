module Doc.Model.Filter
  ( DocumentFilter(..)
  , documentFilterToSQL
  ) where

import Data.Monoid
import Data.Monoid.Space
import DB
import Doc.Model.Expressions
import qualified Control.Monad.State.Lazy as State
import Doc.SealStatus (SealStatus(..))
import User.UserID
import MinutesTime
import Doc.DocumentID
import Control.Logic
import Doc.DocStateData
import Instances ()
import Control.Monad
import Company.Model

-- | Document filtering options
--
-- Each of these options should narrow down set of documents that are
-- visible. Most have some relation to some features of either the
-- document itself or to a signatory link that makes the document
-- available to see.
--
-- This is important: it is not only about document, it is also about
-- the route we got to the document, i. e. signatory_links!.
data DocumentFilter
  = DocumentFilterStatuses [DocumentStatus]   -- ^ Any of listed statuses
  | DocumentFilterBySealStatus [SealStatus]   -- ^ Any of listed seal statuses
  | DocumentFilterByStatusClass [StatusClass] -- ^ Any of listed status classes
  | DocumentFilterByTags [DocumentTag]        -- ^ All of listed tags (warning: this is ALL tags)
  | DocumentFilterByString String             -- ^ Contains the string in title, list of people involved or anywhere
  | DocumentFilterByDelivery DeliveryMethod   -- ^ Only documents that use selected delivery method
  | DocumentFilterByMonthYearFrom (Int,Int)   -- ^ Document time after or in (month,year)
  | DocumentFilterByMonthYearTo   (Int,Int)   -- ^ Document time before or in (month,year)
  | DocumentFilterByAuthor UserID             -- ^ Only documents created by this user
  | DocumentFilterByAuthorCompany CompanyID   -- ^ Onl documents where author is in given company
  | DocumentFilterByCanSign UserID            -- ^ Only if given person can sign right now given document
  | DocumentFilterSignNowOnPad                -- ^ Only documents where someone can sign now on pad
  | DocumentFilterByDocumentID DocumentID     -- ^ Document by specific ID
  | DocumentFilterByDocumentIDs [DocumentID]  -- ^ Documents by specific IDs
  | DocumentFilterSignable                    -- ^ Document is signable
  | DocumentFilterTemplate                    -- ^ Document is template
  | DocumentFilterDeleted Bool                -- ^ Only deleted (=True) or non-deleted (=False) documents.
  | DocumentFilterLinkIsAuthor Bool           -- ^ Only documents visible by signatory_links.is_author equal to param
  | DocumentFilterLinkIsPartner Bool          -- ^ Only documents visible by signatory_links.is_partner equal to param
  | DocumentFilterUnsavedDraft Bool           -- ^ Only documents with unsaved draft flag equal to this one
  | DocumentFilterByModificationTimeAfter MinutesTime -- ^ That were modified after given time
  | DocumentFilterByLatestSignTimeBefore MinutesTime  -- ^ With latest sign time before given time
  | DocumentFilterByLatestSignTimeAfter MinutesTime   -- ^ With latest sign time after given time
  deriving Show

documentFilterToSQL :: (State.MonadState v m, SqlWhere v) => DocumentFilter -> m ()
documentFilterToSQL (DocumentFilterStatuses statuses) = do
  sqlWhereIn "documents.status" statuses
documentFilterToSQL (DocumentFilterBySealStatus statuses) = do
  sqlWhereExists $ sqlSelect "main_files" $ do
    sqlWhere "main_files.document_id = documents.id"
    sqlWhere "main_files.id = (SELECT (max(id)) FROM main_files where document_id = documents.id)"
    sqlWhereIn "main_files.seal_status" statuses


-- Filter on statuses is much faster then on status class. We need to keep it for compatibility reasons
-- but it should be gone with new API version since it's very expensive
documentFilterToSQL (DocumentFilterByStatusClass [SCDraft]) = do
  documentFilterToSQL (DocumentFilterStatuses [Preparation])
documentFilterToSQL (DocumentFilterByStatusClass [SCCancelled,SCRejected,SCTimedout, SCError]) = do
  documentFilterToSQL (DocumentFilterStatuses [Canceled, Rejected, Timedout, DocumentError ""])
documentFilterToSQL (DocumentFilterByStatusClass [SCSent,SCDelivered,SCRead,SCOpened, SCDeliveryProblem]) = do
  documentFilterToSQL (DocumentFilterStatuses [Pending])
documentFilterToSQL (DocumentFilterByStatusClass [SCSigned]) = do
  documentFilterToSQL (DocumentFilterStatuses [Closed])

documentFilterToSQL (DocumentFilterByStatusClass statuses) = do
  -- I think here we can use the result that we define on select
  -- check this one out later
  sqlWhereIn documentStatusClassExpression statuses

documentFilterToSQL (DocumentFilterByModificationTimeAfter mtime) = do
  sqlWhere ("(SELECT max(greatest(signatory_links.sign_time"
            <> ", signatory_links.seen_time"
            <> ", signatory_links.read_invitation"
            <> ", documents.invite_time"
            <> ", signatory_links.rejection_time"
            <> ", documents.mtime"
            <> ", documents.ctime"
            <> ")) FROM signatory_links WHERE signatory_links.document_id = documents.id)"
            <+> ">=" <?> mtime)

documentFilterToSQL (DocumentFilterByLatestSignTimeBefore time) = do
  sqlWhere $ documentLatestSignTimeExpression <+> "<" <?> time
documentFilterToSQL (DocumentFilterByLatestSignTimeAfter time) = do
  sqlWhere $ documentLatestSignTimeExpression <+> ">" <?> time
documentFilterToSQL (DocumentFilterByMonthYearFrom (month,year)) = do
  sqlWhere $ raw $ unsafeSQL $ "(documents.mtime > '" ++ show year ++  "-" ++ show month ++ "-1')"
documentFilterToSQL (DocumentFilterByMonthYearTo (month,year)) = do
  sqlWhere $ raw $ unsafeSQL $ "(documents.mtime < '" ++ show (year + 1 <| month == 12 |> year)++ "-" ++ show ((month `mod` 12) + 1) ++ "-1')"
documentFilterToSQL (DocumentFilterByTags []) = do
  sqlWhere "TRUE"
documentFilterToSQL (DocumentFilterByTags tags) = do
  forM_ tags $ \tag -> do
    sqlWhereExists $ sqlSelect "document_tags" $ do
      sqlWhere "documents.id = document_tags.document_id"
      sqlWhereEq "document_tags.name" (tagname tag)
      sqlWhereEq "document_tags.value" (tagvalue tag)
documentFilterToSQL (DocumentFilterByString string) = do
  sqlWhere result
  where
      result = parenthesize $ (("documents.title ILIKE" <?> sqlpat string) `sqlOR`
                               sqlConcatAND (map sqlMatch (words string)))
      sqlMatch word = "EXISTS (SELECT TRUE" <>
                                   "  FROM signatory_link_fields JOIN signatory_links AS sl5" <>
                                                                 "  ON sl5.document_id = signatory_links.document_id" <>
                                                                 " AND sl5.id = signatory_link_fields.signatory_link_id" <>
                                   -- " FROM signatory_link_fields " <>
                                   " WHERE (signatory_link_fields.type != " <?> SignatureFT "signature" <> ") " <>
                                           " AND (signatory_link_fields.value ILIKE" <?> sqlpat word <> "))"
                                   --" WHERE TRUE)") []

      sqlpat text = "%" ++ concatMap escape text ++ "%"
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

documentFilterToSQL (DocumentFilterByDelivery delivery) = do
  sqlWhereEq "documents.delivery_method" delivery

documentFilterToSQL (DocumentFilterLinkIsAuthor flag) = do
  sqlWhereEq "signatory_links.is_author" flag

documentFilterToSQL (DocumentFilterLinkIsPartner flag) = do
  sqlWhereEq "signatory_links.is_partner" flag

documentFilterToSQL (DocumentFilterUnsavedDraft flag) =
  sqlWhereAny $ do
    sqlWhereEq "documents.unsaved_draft" flag
    sqlWhereNotEq "documents.type" Signable
    sqlWhereNotEq "documents.status" Preparation

documentFilterToSQL (DocumentFilterByAuthor userid) = do
  sqlWhere "signatory_links.is_author"
  sqlWhereEq "signatory_links.user_id" userid

documentFilterToSQL (DocumentFilterByAuthorCompany companyid) = do
  sqlWhere "signatory_links.is_author"
  sqlWhere "signatory_links.user_id = users.id"
  sqlWhereEq "users.company_id" companyid


documentFilterToSQL (DocumentFilterByCanSign userid) = do
  sqlWhere "signatory_links.is_partner"
  sqlWhereEq "signatory_links.user_id" userid
  sqlWhereEq "documents.status" Pending
  sqlWhereIsNULL "signatory_links.sign_time"
  sqlWhereEqSql "signatory_links.sign_order" documentSignorderExpression

documentFilterToSQL (DocumentFilterSignNowOnPad) = do
  sqlWhereEq "documents.status" Pending
  sqlWhereExists $ sqlSelect "signatory_links AS sl5" $ do
                      sqlWhere "sl5.document_id = signatory_links.document_id"
                      sqlWhere "sl5.is_partner"
                      sqlWhereIsNULL "sl5.sign_time"
                      sqlWhereEqSql "sl5.sign_order" documentSignorderExpression
                      sqlWhereEq "sl5.delivery_method" PadDelivery

documentFilterToSQL (DocumentFilterByDocumentID did) = do
  sqlWhereEq "documents.id" did

documentFilterToSQL (DocumentFilterByDocumentIDs dids) = do
  sqlWhereIn "documents.id" dids

documentFilterToSQL (DocumentFilterSignable) = do
  sqlWhereEq "documents.type" Signable

documentFilterToSQL (DocumentFilterTemplate) = do
  sqlWhereEq "documents.type" Template

documentFilterToSQL (DocumentFilterDeleted flag1) = do
  sqlWhere "signatory_links.really_deleted IS NULL"
  if flag1
     then sqlWhere "signatory_links.deleted IS NOT NULL"
     else sqlWhere "signatory_links.deleted IS NULL"
