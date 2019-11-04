module Doc.Model.Filter
  ( DocumentFilter(..)
  , documentFilterToSQL
  , FilterString(..) -- Exported for Tests
  , processSearchStringToFilter
  ) where

import Control.Conditional ((<|), (|>))
import qualified Control.Monad.State.Lazy as State
import qualified Data.Text as T

import DB
import Doc.Conditions
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model.Expressions
import Folder.Types
import MinutesTime
import TextSearchQuery
import User.UserID

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
  | DocumentFilterByStatusClass [StatusClass] -- ^ Any of listed status classes
  | DocumentFilterByTags [DocumentTag]        -- ^ All of listed tags (warning: this is ALL tags)
  | DocumentFilterByTSQuery [FilterString]    -- ^ Uses FTS search on title and the values of signatory fields
  | DocumentFilterByDelivery DeliveryMethod   -- ^ Only documents that use selected delivery method
  | DocumentFilterByMonthYearFrom (Int,Int)   -- ^ Document time after or in (month,year)
  | DocumentFilterByMonthYearTo   (Int,Int)   -- ^ Document time before or in (month,year)
  | DocumentFilterByTimeAfter UTCTime         -- ^ Document time after UTC time
  | DocumentFilterByTimeBefore UTCTime        -- ^ Document time before UTC time
  | DocumentFilterByAuthor UserID             -- ^ Only documents created by this user
  | DocumentFilterByCanSign UserID            -- ^ Only if given person can sign right now given document
  | DocumentFilterSignNowOnPad                -- ^ Only documents where someone can sign now on pad
  | DocumentFilterByDocumentID DocumentID     -- ^ Document by specific ID
  | DocumentFilterByDocumentIDs [DocumentID]  -- ^ Documents by specific IDs
  | DocumentFilterSignable                    -- ^ Document is signable
  | DocumentFilterTemplate                    -- ^ Document is template
  | DocumentFilterDeleted Bool                -- ^ Only deleted (=True) or non-deleted (=False) documents.
  | DocumentFilterLinkIsAuthor Bool           -- ^ Only documents visible by signatory_links.is_author equal to param
  | DocumentFilterUnsavedDraft Bool           -- ^ Only documents with unsaved draft flag equal to this one
  | DocumentFilterByModificationTimeAfter UTCTime -- ^ That were modified after given time
  | DocumentFilterByFolderID FolderID         -- ^ Documents in given folder
  deriving Show

documentFilterToSQL :: (State.MonadState v m, SqlWhere v) => DocumentFilter -> m ()
documentFilterToSQL (DocumentFilterStatuses statuses) = do
  sqlWhereIn "documents.status" statuses

-- Filter on statuses is much faster then on status class. We need to keep it for compatibility reasons
-- but it should be gone with new API version since it's very expensive
documentFilterToSQL (DocumentFilterByStatusClass [SCDraft]) = do
  documentFilterToSQL (DocumentFilterStatuses [Preparation])
documentFilterToSQL (DocumentFilterByStatusClass [SCCancelled, SCRejected, SCTimedout, SCError])
  = do
    documentFilterToSQL
      (DocumentFilterStatuses [Canceled, Rejected, Timedout, DocumentError])
documentFilterToSQL (DocumentFilterByStatusClass [SCSent, SCDelivered, SCRead, SCOpened, SCDeliveryProblem])
  = do
    documentFilterToSQL (DocumentFilterStatuses [Pending])
documentFilterToSQL (DocumentFilterByStatusClass [SCSigned]) = do
  documentFilterToSQL (DocumentFilterStatuses [Closed])

documentFilterToSQL (DocumentFilterByStatusClass statuses) = do
  -- I think here we can use the result that we define on select
  -- check this one out later
  sqlWhereIn documentStatusClassExpression statuses

documentFilterToSQL (DocumentFilterByModificationTimeAfter mtime) = do
  sqlWhere
    (   "(SELECT max(greatest(signatory_links.sign_time"
    <>  ", signatory_links.seen_time"
    <>  ", signatory_links.read_invitation"
    <>  ", documents.invite_time"
    <>  ", signatory_links.rejection_time"
    <>  ", documents.mtime"
    <>  ", documents.ctime"
    <>  ")) FROM signatory_links WHERE signatory_links.document_id = documents.id)"
    <+> ">="
    <?> mtime
    )

documentFilterToSQL (DocumentFilterByMonthYearFrom (month, year)) = do
  sqlWhere
    $  raw
    $  unsafeSQL
    $  "(documents.mtime > '"
    ++ show year
    ++ "-"
    ++ show month
    ++ "-1')"
documentFilterToSQL (DocumentFilterByMonthYearTo (month, year)) = do
  sqlWhere
    $  raw
    $  unsafeSQL
    $  "(documents.mtime < '"
    ++ show (year + 1 <| month == 12 |> year)
    ++ "-"
    ++ show ((month `mod` 12) + 1)
    ++ "-1')"
documentFilterToSQL (DocumentFilterByTimeAfter time) = do
  sqlWhere $ "documents.mtime >=" <?> time
documentFilterToSQL (DocumentFilterByTimeBefore time) = do
  sqlWhere $ "documents.mtime <=" <?> time
documentFilterToSQL (DocumentFilterByTags []) = do
  sqlWhere "TRUE"
documentFilterToSQL (DocumentFilterByTags tags) = do
  forM_ tags $ \tag -> do
    sqlWhereExists $ sqlSelect "document_tags" $ do
      sqlWhere "documents.id = document_tags.document_id"
      sqlWhereEq "document_tags.name"  (tagname tag)
      sqlWhereEq "document_tags.value" (tagvalue tag)

documentFilterToSQL (DocumentFilterByTSQuery qry) =
  case mapMaybe interpretFilterString qry of
    [] -> do
      return ()
    (qryTerm : qryTerms) -> do
      let tsQry = foldl' (:&&:) qryTerm qryTerms
      sqlWhere $ parenthesize ("documents.archive_search_fts" <@@> tsQry)

documentFilterToSQL (DocumentFilterByDelivery delivery) = do
  sqlWhereEq "documents.delivery_method" delivery

documentFilterToSQL (DocumentFilterLinkIsAuthor flag) = do
  sqlWhere $ "documents.author_id" <+> op <+> "signatory_links.id"
  where op = if flag then "=" else "<>"

documentFilterToSQL (DocumentFilterUnsavedDraft flag) = sqlWhereAny
  [ sqlWhereEq "documents.unsaved_draft" flag
  , sqlWhereNotEq "documents.type"   Signable
  , sqlWhereNotEq "documents.status" Preparation
  ]

documentFilterToSQL (DocumentFilterByAuthor userid) = do
  sqlWhere "documents.author_id = signatory_links.id"
  sqlWhereEq "signatory_links.user_id" userid

documentFilterToSQL (DocumentFilterByCanSign userid) = do
  sqlWhereSignatoryRoleIsSigningParty
  sqlWhereEq "signatory_links.user_id" userid
  sqlWhereEq "documents.status"        Pending
  sqlWhereIsNULL "signatory_links.sign_time"
  sqlWhereEqSql "signatory_links.sign_order" documentSignOrderExpression

documentFilterToSQL (DocumentFilterSignNowOnPad) = do
  sqlWhereEq "documents.status" Pending
  sqlWhereExists $ sqlSelect "signatory_links AS sl5" $ do
    sqlWhere "sl5.document_id = signatory_links.document_id"
    sqlWhere $ "sl5.signatory_role =" <?> SignatoryRoleSigningParty
    sqlWhereIsNULL "sl5.sign_time"
    sqlWhereEqSql "sl5.sign_order" documentSignOrderExpression
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

documentFilterToSQL (DocumentFilterByFolderID fid) = do
  sqlWhereEq "documents.folder_id" fid

data FilterString = Quoted Text | Unquoted Text
  deriving (Show, Eq)

-- | Converts a search string into a `DocumentFilterByTSQuery [FilterString]`
--
-- Search string is split into words, but anything between quotes is treated as
-- a literal search and gets Quoted.
-- In case of mismatch quotes, last quote is ignored and we just use
-- Unquoted for all words.
--
-- >>> processSearchStringToFilter "my search"
-- DocumentFilterByTSQuery [Unquoted "my", Unquoted "search"]
--
-- >>> processSearchStringToFilter "my search \"for life\""
-- DocumentFilterByTSQuery [Unquoted "my", Unquoted "search", Quoted "for life"]
--
processSearchStringToFilter :: Text -> DocumentFilter
processSearchStringToFilter str = DocumentFilterByTSQuery . take 5 . convert $ str
  where
    convert s = mergeAroundQuotes [] Nothing (T.words $ spaceAroundQuotes s)
    spaceAroundQuotes s =
      T.concatMap (\c -> if c == '"' then " \" " else T.singleton c) s
    -- Usage: mergeAroundQuotes [] Nothing yourWords
    -- Expects a list of words, where quotation marks (") are their own word
    -- Collapses words within quotation marks into a single space-delimited word
    -- Ignores unmatched quotes
    mergeAroundQuotes :: [FilterString] -> Maybe [Text] -> [Text] -> [FilterString]
    mergeAroundQuotes acc Nothing  []          = acc
    mergeAroundQuotes acc (Just q) []          = acc ++ (map Unquoted q)
    mergeAroundQuotes acc Nothing  ("\"" : ws) = mergeAroundQuotes acc (Just []) ws
    mergeAroundQuotes acc Nothing (w : ws) =
      mergeAroundQuotes (acc ++ [Unquoted w]) Nothing ws
    mergeAroundQuotes acc (Just q) ("\"" : ws) = if not . null $ q
      then mergeAroundQuotes (acc ++ [Quoted $ T.intercalate " " q]) Nothing ws
      else mergeAroundQuotes acc Nothing ws
    mergeAroundQuotes acc (Just q) (w : ws) = mergeAroundQuotes acc (Just $ q ++ [w]) ws

-- TODO To support phrase searches we should really `foldl'` on `(:<->:)` in
-- the `Quoted t` case, but since we're stuck with PostgreSQL 9.5 for a
-- while yet we use `(:&&:)` instead; in other words, phrase search is not
-- yet supported. Update when $PG_VERSION >= 9.6 in all installations (good
-- luck!).
interpretFilterString :: FilterString -> Maybe TSQuery
interpretFilterString (Unquoted t) = Just . mkTerm $ t
interpretFilterString (Quoted   t) = mkSimplePhraseLegacy t
