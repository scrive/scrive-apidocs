{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Model.Query
  ( isTemplate -- fromUtils
  , DocumentFilter(..)
  , processSearchStringToFilter
  , DocumentDomain(..)
  , DocumentOrderBy(..)
  , FileInDocument(..)
  , GetDocument(..)
  , GetDocuments(..)
  , GetDocumentsWithSoftLimit(..)
  , GetDocumentsIDs(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentForDave(..)
  , GetDocumentBySignatoryLinkID(..)
  , GetDocumentsBySignatoryLinkIDs(..)
  , GetDocumentByDocumentIDSignatoryLinkIDMagicHash(..)
  , GetDocumentsByAuthor(..)
  , GetSignatoryScreenshots(..)
  , GetSignatoryLinkByID(..)
  , GetTemplatesByAuthor(..)
  , GetAvailableTemplates(..)
  , GetTimeoutedButPendingDocumentsChunk(..)
  , GetDocsSentBetween(..)
  , GetDocsSent(..)
  , GetDocumentTags(..)
  , CheckDocumentObjectVersionIs(..)
  , DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State
import Data.Int
import Data.List hiding (tail, head)
import Data.Maybe hiding (fromJust)
import Log
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as S

import Company.CompanyID
import DB
import DB.RowCache (GetRow(..))
import Doc.Conditions
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocUtils
import Doc.Model.Domain
import Doc.Model.Filter
import Doc.Model.OrderBy
import Doc.Screenshot
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import File.FileID
import File.Storage
import KontraPrelude
import MagicHash
import User.Model
import qualified Amazon

data GetSignatoryScreenshots = GetSignatoryScreenshots [SignatoryLinkID]
instance (MonadDB m, MonadThrow m, MonadLog m, MonadBase IO m, Amazon.AmazonMonad m) => DBQuery m GetSignatoryScreenshots [(SignatoryLinkID, SignatoryScreenshots)] where
  query (GetSignatoryScreenshots l) = do
    runQuery_ . sqlSelect "signatory_screenshots" $ do
                sqlWhereIn "signatory_link_id" l
                sqlOrderBy "signatory_link_id"

                sqlResult "signatory_link_id"
                sqlResult "type"
                sqlResult "time"
                sqlResult "file_id"
    screenshotsWithoutBinaryData <- fetchMany id
    let getBinaries (slid, ty, time, fid) = do
           bin <- getFileIDContents fid
           return (slid, ty, time, Binary bin)
    screenshotsWithBinaryData <- mapM getBinaries screenshotsWithoutBinaryData

    let folder ((slid', s):a) (slid, ty, time, i) | slid' == slid = (slid, mkss ty time i s):a
        folder a (slid, ty, time, i) = (slid, mkss ty time i emptySignatoryScreenshots) : a

        mkss :: String -> UTCTime -> Binary BS.ByteString -> SignatoryScreenshots -> SignatoryScreenshots
        mkss "first"     time i s = s{ first = Just $ Screenshot time i }
        mkss "signing"   time i s = s{ signing = Just $ Screenshot time i }
        mkss "reference" time i s = s{ reference = Just $ Right $ Screenshot time i }
        mkss t           _    _ _ = $unexpectedError $ "invalid type: " <> show t
    return $ foldl' folder [] screenshotsWithBinaryData

data FileInDocument = FileInDocument DocumentID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m FileInDocument Bool where
  query (FileInDocument did fid) = do
    let s1 = sqlSelect "main_files" $ do
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    let s2 = sqlSelect "author_attachments" $ do
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    let s3 = sqlSelect "signatory_attachments" $ do
                   sqlJoinOn "signatory_links" "signatory_attachments.signatory_link_id = signatory_links.id"
                   sqlWhereEq "file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    let s4 = sqlSelect "signatory_link_fields" $ do
                   sqlJoinOn "signatory_links" "signatory_link_fields.signatory_link_id = signatory_links.id"
                   sqlWhereEq "value_file_id" fid
                   sqlWhereEq "document_id" did
                   sqlResult "TRUE"
    runQuery_ $ "SELECT EXISTS (" <> toSQLCommand s1 <> ") OR " <>
                       "EXISTS (" <> toSQLCommand s2 <> ") OR " <>
                       "EXISTS (" <> toSQLCommand s3 <> ") OR " <>
                       "EXISTS (" <> toSQLCommand s4 <> ")"
    fetchOne runIdentity

data GetDocumentTags = GetDocumentTags DocumentID
instance MonadDB m => DBQuery m GetDocumentTags (S.Set DocumentTag) where
  query (GetDocumentTags did) = do
    runQuery_ $ sqlSelect "document_tags" $ do
      mapM_ sqlResult documentTagsSelectors
      sqlWhereEq "document_id" did
    S.fromList <$> fetchMany toComposite

data GetSignatoryLinkByID = GetSignatoryLinkByID DocumentID SignatoryLinkID (Maybe MagicHash)
instance (MonadDB m, MonadThrow m) => DBQuery m GetSignatoryLinkByID SignatoryLink where
  query (GetSignatoryLinkByID did slid mmh) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "signatory_links" $ do
      sqlJoinOn "documents" "signatory_links.document_id = documents.id"
      mapM_ sqlResult signatoryLinksSelectors
      sqlWhereDocumentIDForSignatoryIs did
      sqlWhereSignatoryLinkIDIs slid
      F.mapM_ sqlWhereSignatoryLinkMagicHashIs mmh

selectDocuments :: DocumentDomain
                -> [DocumentFilter]
                -> [AscDesc DocumentOrderBy]
                -> Int
                -> State SqlSelect ()
                -> SqlSelect
selectDocuments domain filters orders limit extend = sqlSelect "documents" $ do
  -- We want to inject filters, offset and limit into domain as soon
  -- as possible to avoid fetching unnecessary rows.
  sqlWith "visible_document_ids" . sqlSelect "domain_ids" $ do
    sqlWith "domain_ids" . sqlSelect "documents" $ do
      -- When fetching documents of the whole universe don't apply
      -- DISTINCT as it prevents usage of mtime index (when using
      -- default sort order), which results in poor performance.
      when (domain /= DocumentsOfWholeUniverse) $ do
        sqlDistinct
      mapM_ sqlResult $ "documents.id"
        -- Include sort expressions as DISTINCT demands it.
        : map (\dobr -> dobrExpr dobr <+> "AS" <+> dobrName dobr) orderList
      documentDomainToSQL domain
      mapM_ documentFilterToSQL filters
      mapM_ (sqlOrderBy . (\dobr -> dobrName dobr <+> dobrOrder dobr)) orderList
      sqlLimit limit
    -- Enumerate rows only if order is specified.
    when orderSpecified $ do
      sqlResult "ROW_NUMBER() OVER() AS position"
    sqlResult "domain_ids.id"
  sqlJoinOn "visible_document_ids" "documents.id = visible_document_ids.id"
  -- Retain original order only if it was specified in the first place.
  when orderSpecified $ do
    sqlOrderBy "visible_document_ids.position"
  extend
  where
    orderSpecified = not $ null orders
    orderList = map documentOrderByAscDescToSQL orders

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentByDocumentID Document where
  query (GetDocumentByDocumentID did) = do
    -- FIXME: Use domains/filters.
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereDocumentWasNotPurged

-- Like GetDocumentByDocumentID, but also retrieves purged docs,
-- only to be used for Dave
data GetDocumentForDave = GetDocumentForDave DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentForDave Document where
  query (GetDocumentForDave did) = do
    -- FIXME: Use domains/filters.
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did

data GetDocumentBySignatoryLinkID = GetDocumentBySignatoryLinkID SignatoryLinkID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentBySignatoryLinkID Document where
  query (GetDocumentBySignatoryLinkID slid) = do
    -- FIXME: Use domains/filters.
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereEqSql "documents.id" . parenthesize . toSQLCommand . sqlSelect "signatory_links" $ do
        sqlResult "signatory_links.document_id"
        sqlWhereEq "signatory_links.id" slid
      sqlWhereDocumentWasNotPurged

data GetDocumentsBySignatoryLinkIDs = GetDocumentsBySignatoryLinkIDs [SignatoryLinkID]
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentsBySignatoryLinkIDs [Document] where
  query (GetDocumentsBySignatoryLinkIDs slids) = do
    -- FIXME: Use domains/filters.
    runQuery_ . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhereIn "signatory_links.id" slids
        sqlWhere "signatory_links.document_id = documents.id"
      sqlWhereDocumentWasNotPurged
    fetchMany toComposite

data GetDocumentByDocumentIDSignatoryLinkIDMagicHash = GetDocumentByDocumentIDSignatoryLinkIDMagicHash DocumentID SignatoryLinkID MagicHash
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentByDocumentIDSignatoryLinkIDMagicHash Document where
  query (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) = do
    -- FIXME: Use domains/filters.
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereExists $ sqlSelect "signatory_links" $ do
         sqlWhere "signatory_links.document_id = documents.id"
         -- Thought for later: Here we might actually check if
         -- visibility rules allow a person to see this document, for
         -- example if sign order allows to see the document. For now
         -- we are sloppy and let a person see the document.
         sqlWhereSignatoryLinkIDIs slid
         sqlWhereSignatoryLinkMagicHashIs mh
      sqlWhereDocumentWasNotPurged

-- | GetDocuments is central switch for documents list queries.
--
-- GetDocuments domains filters sorting pagination
--
-- * domains are connected with OR, so documents falling into ANY of domains will be returned
-- * filters weed out documents from domains, are connected with AND so a document must pass through ALL filters
-- * sortings returns documents in order
-- * pagination is a place to put OFFSET and LIMIT values
--
-- GetDocuments returns documents in proper order, no reverse is needed.
--

data GetDocuments = GetDocuments DocumentDomain [DocumentFilter] [AscDesc DocumentOrderBy] Int
instance MonadDB m => DBQuery m GetDocuments [Document] where
  query (GetDocuments domain filters orders limit) = do
    runQuery_ . selectDocuments domain filters orders limit $ do
      mapM_ sqlResult documentsSelectors
    fetchMany toComposite

data GetDocument = GetDocument DocumentDomain [DocumentFilter]
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocument Document where
  query (GetDocument domain filters) = do
    -- Set limit to 2 so it can throw if more than 1 row is returned.
    kRunAndFetch1OrThrowWhyNot toComposite . selectDocuments domain filters [] 2 $ do
      mapM_ sqlResult documentsSelectors

data GetDocumentsWithSoftLimit = GetDocumentsWithSoftLimit DocumentDomain [DocumentFilter] [AscDesc DocumentOrderBy] (Int, Int, Int)
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentsWithSoftLimit (Int, [Document]) where
  query (GetDocumentsWithSoftLimit domain filters orders (offset, limit, soft_limit)) = do
    --analysis <- explainAnalyze $ toSQLCommand sql
    --trace ("ANALYSIS:" <+> analysis) $ return ()
    --trace (show $ toSQLCommand sql) $ return ()
    runQuery_ sql
    (count :: Int64, CompositeArray1 documents) <- fetchOne id
    return (fromIntegral count, documents)
    where
      sql = sqlSelect "" $ do
        -- get ids of documents for total count
        sqlWith "selected_ids" . selectDocuments domain filters orders (offset + limit) $ do
          sqlResult "documents.id AS id"
        -- restrict them with the soft limit
        sqlWith "relevant_ids" . sqlSelect "selected_ids" $ do
          sqlResult "ROW_NUMBER() OVER() AS position"
          sqlResult "id"
          sqlOffset offset
          sqlLimit soft_limit
        -- fetch total count of documents
        sqlResult $ "(SELECT COUNT(*) FROM selected_ids) AS total_count"
        -- and a list of them, restricted by the soft limit
        sqlResult $ "ARRAY(SELECT (" <> mintercalate ", " documentsSelectors <> ")::document FROM relevant_ids ids JOIN documents USING (id) ORDER BY ids.position) AS documents"

data GetDocumentsIDs = GetDocumentsIDs DocumentDomain [DocumentFilter] [AscDesc DocumentOrderBy]
instance MonadDB m => DBQuery m GetDocumentsIDs [DocumentID] where
  query (GetDocumentsIDs domain filters orders) = do
    let hardLimit = 10000
    runQuery_ . selectDocuments domain filters orders hardLimit $ do
      sqlResult "documents.id"
    fetchMany runIdentity

-- | All documents authored by the user that have never been deleted.
data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentsByAuthor [Document] where
  query (GetDocumentsByAuthor uid) = query $ GetDocuments
    (DocumentsVisibleToUser uid)
    [DocumentFilterByAuthor uid, DocumentFilterDeleted False]
    [Asc DocumentOrderByMTime]
    (-1)

data GetTemplatesByAuthor = GetTemplatesByAuthor UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetTemplatesByAuthor [Document] where
  query (GetTemplatesByAuthor uid) = query $ GetDocuments
    (DocumentsVisibleToUser uid)
    [DocumentFilterByAuthor uid, DocumentFilterDeleted False, DocumentFilterTemplate]
    [Asc DocumentOrderByMTime]
    (-1)

data GetAvailableTemplates = GetAvailableTemplates UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetAvailableTemplates [Document] where
  query (GetAvailableTemplates uid) = query $ GetDocuments
    (DocumentsVisibleToUser uid)
    [DocumentFilterTemplate, DocumentFilterDeleted False]
    [Asc DocumentOrderByMTime]
    (-1)

data GetTimeoutedButPendingDocumentsChunk = GetTimeoutedButPendingDocumentsChunk UTCTime Int
instance (MonadDB m, MonadThrow m) => DBQuery m GetTimeoutedButPendingDocumentsChunk [Document] where
  query (GetTimeoutedButPendingDocumentsChunk mtime size) = do
    runQuery_ . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereEq "documents.status" Pending
      sqlWhere "purged_time IS NULL"
      sqlWhere $ "timeout_time IS NOT NULL AND timeout_time < " <?> mtime
      sqlLimit size
    fetchMany toComposite

data GetDocsSentBetween = GetDocsSentBetween CompanyID UTCTime UTCTime
instance MonadDB m => DBQuery m GetDocsSentBetween Int64 where
  query (GetDocsSentBetween cid start end) = do
    runQuery_ $ "SELECT count(documents.id) " <>
               "FROM documents " <>
               "JOIN signatory_links ON documents.id = signatory_links.document_id " <>
               "JOIN users ON signatory_links.user_id = users.id " <>
               "WHERE users.company_id =" <?> cid <>
               "AND documents.author_id = signatory_links.id " <>
               "AND documents.invite_time >=" <?> start <>
               "AND documents.invite_time <" <?> end <>
               "AND documents.type =" <?> Signable <>
               "AND documents.status <>" <?> Preparation
    foldlDB (\acc (Identity n) -> return $ acc + n) 0

data GetDocsSent = GetDocsSent UserID
instance MonadDB m => DBQuery m GetDocsSent Int64 where
  query (GetDocsSent uid) = do
    runQuery_ $ "SELECT count(documents.id) " <>
               "FROM documents " <>
               "JOIN signatory_links ON documents.id = signatory_links.document_id " <>
               "WHERE signatory_links.user_id =" <?> uid <>
               "AND documents.author_id = signatory_links.id " <>
               "AND documents.type =" <?> Signable <>
               "AND documents.status <>" <?> Preparation
    foldlDB (\acc (Identity n) -> return $ acc + n) 0

data CheckDocumentObjectVersionIs = CheckDocumentObjectVersionIs DocumentID Int64
instance (MonadDB m, MonadThrow m) => DBQuery m CheckDocumentObjectVersionIs () where
  query (CheckDocumentObjectVersionIs did ov) = do
    _ :: Bool <- kRunAndFetch1OrThrowWhyNot runIdentity $ sqlSelect "documents" $ do
       sqlResult "TRUE"
       sqlWhereDocumentIDIs did
       sqlWhereDocumentObjectVersionIs ov
    return ()

data DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor = DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor Bool where
  query (DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor did) = do
    runQuery_ . sqlSelect "documents" $ do
      sqlResult "TRUE"
      sqlWhereDocumentIDIs did
      sqlWhereDocumentWasNotPurged
      sqlWhereNotExists $ sqlSelect "signatory_links" $ do
        sqlWhere "documents.author_id = signatory_links.id"
        sqlWhere "signatory_links.really_deleted IS NOT NULL"
        sqlWhere "signatory_links.document_id = documents.id"
    result <- fetchMaybe runIdentity
    return $ result == Just True

instance (MonadDB m, MonadThrow m) => GetRow Document m where
  getRow did = dbQuery $ GetDocumentByDocumentID did
