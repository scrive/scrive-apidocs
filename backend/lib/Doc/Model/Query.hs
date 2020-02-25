{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Model.Query
  ( isTemplate -- from Doc.DocInfo
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
  , GetDocumentByShortDocumentID(..)
  , GetDocumentForDave(..)
  , GetDocumentBySignatoryLinkID(..)
  , GetDocumentsBySignatoryLinkIDs(..)
  , GetDocumentByDocumentIDSignatoryLinkID(..)
  , GetDocumentByDocumentIDSignatoryLinkIDMagicHash(..)
  , GetDocumentByDocumentIDAndShareableLinkHash(..)
  , CheckIfMagicHashIsValid(..)
  , GetDocumentsByAuthor(..)
  , GetSignatoryScreenshots(..)
  , GetSignatoryLinkByID(..)
  , GetTemplatesByAuthor(..)
  , GetAvailableTemplates(..)
  , GetTimeoutedButPendingDocumentsChunk(..)
  , GetDocumentTags(..)
  , CheckDocumentObjectVersionIs(..)
  , DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor(..)
  , GetRandomSignatoryLinkIDThatSignedRecently(..)
  , GetDocumentAPICallbackResult(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Int
import Log
import qualified Data.ByteString as BS
import qualified Data.Set as S

import DB
import DB.RowCache (GetRow(..))
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model.Domain
import Doc.Model.Filter
import Doc.Model.OrderBy
import Doc.Screenshot
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
import Doc.Tables
import File.FileID
import File.Storage
import MagicHash
import User.Model
import Util.SignatoryLinkUtils

data GetSignatoryScreenshots = GetSignatoryScreenshots [SignatoryLinkID]
instance (MonadDB m, MonadIO m, MonadMask m, MonadLog m, MonadBaseControl IO m, MonadFileStorage m) => DBQuery m GetSignatoryScreenshots [(SignatoryLinkID, SignatoryScreenshots)] where
  query (GetSignatoryScreenshots l) = do
    runQuery_ . sqlSelect "signatory_screenshots" $ do
      sqlWhereIn "signatory_link_id" l
      sqlOrderBy "signatory_link_id"

      sqlResult "signatory_link_id"
      sqlResult "type"
      sqlResult "time"
      sqlResult "file_id"
    screenshotsWithoutBinaryData <- fetchMany identity
    let getBinaries (slid, ty, time, fid) = do
          bin <- getFileIDContents fid
          return (slid, ty, time, bin)
    screenshotsWithBinaryData <- mapM getBinaries screenshotsWithoutBinaryData

    let
      folder ((slid', s) : a) (slid, ty, time, i) | slid' == slid =
        (slid, mkss ty time i s) : a
      folder a (slid, ty, time, i) = (slid, mkss ty time i emptySignatoryScreenshots) : a

      mkss
        :: String
        -> UTCTime
        -> BS.ByteString
        -> SignatoryScreenshots
        -> SignatoryScreenshots
      mkss "first"     time i s = s { first = Just $ Screenshot time i }
      mkss "signing"   time i s = s { signing = Just $ Screenshot time i }
      mkss "reference" time i s = s { reference = Just $ Right $ Screenshot time i }
      mkss t           _    _ _ = unexpectedError $ "invalid type: " <> showt t
    return $ foldl' folder [] screenshotsWithBinaryData

data FileInDocument = FileInDocument DocumentID FileID
instance (MonadDB m, MonadThrow m) => DBQuery m FileInDocument Bool where
  query (FileInDocument did fid) = do
    let s1 = sqlSelect "main_files" $ do
          sqlWhereEq "file_id"     fid
          sqlWhereEq "document_id" did
          sqlResult "TRUE"
    let s2 = sqlSelect "author_attachments" $ do
          sqlWhereEq "file_id"     fid
          sqlWhereEq "document_id" did
          sqlResult "TRUE"
    let s3 = sqlSelect "signatory_attachments" $ do
          sqlJoinOn "signatory_links"
                    "signatory_attachments.signatory_link_id = signatory_links.id"
          sqlWhereEq "file_id"     fid
          sqlWhereEq "document_id" did
          sqlResult "TRUE"
    let s4 = sqlSelect "signatory_link_fields" $ do
          sqlJoinOn "signatory_links"
                    "signatory_link_fields.signatory_link_id = signatory_links.id"
          sqlWhereEq "value_file_id" fid
          sqlWhereEq "document_id"   did
          sqlResult "TRUE"
    let s5 = sqlSelect "highlighted_pages" $ do
          sqlJoinOn "signatory_links"
                    "highlighted_pages.signatory_link_id = signatory_links.id"
          sqlWhereEq "file_id"     fid
          sqlWhereEq "document_id" did
          sqlResult "TRUE"
    runQuery_
      $  "SELECT EXISTS ("
      <> toSQLCommand s1
      <> ") OR "
      <> "EXISTS ("
      <> toSQLCommand s2
      <> ") OR "
      <> "EXISTS ("
      <> toSQLCommand s3
      <> ") OR "
      <> "EXISTS ("
      <> toSQLCommand s4
      <> ") OR "
      <> "EXISTS ("
      <> toSQLCommand s5
      <> ")"
    fetchOne runIdentity

data GetDocumentTags = GetDocumentTags DocumentID
instance MonadDB m => DBQuery m GetDocumentTags (S.Set DocumentTag) where
  query (GetDocumentTags did) = do
    runQuery_ $ sqlSelect "document_tags" $ do
      mapM_ sqlResult documentTagsSelectors
      sqlWhereEq "document_id" did
    S.fromList <$> fetchMany toComposite

data GetSignatoryLinkByID =
  GetSignatoryLinkByID DocumentID SignatoryLinkID

instance (MonadDB m, MonadThrow m) =>
  DBQuery m GetSignatoryLinkByID SignatoryLink where
  query (GetSignatoryLinkByID did slid) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "signatory_links" $ do
      sqlJoinOn "documents" "signatory_links.document_id = documents.id"
      mapM_ sqlResult signatoryLinksSelectors
      sqlWhereDocumentIDForSignatoryIs did
      sqlWhereSignatoryLinkIDIs slid

selectDocuments
  :: DocumentDomain
  -> [DocumentFilter]
  -> [AscDesc DocumentOrderBy]
  -> Int
  -> State SqlSelect ()
  -> SqlSelect
selectDocuments docDomain docFilters orders limit extend = sqlSelect "documents" $ do
  -- We want to inject filters, offset and limit into domain as soon
  -- as possible to avoid fetching unnecessary rows.
  sqlWith "visible_document_ids" . sqlSelect "domain_ids" $ do
    sqlWith "domain_ids" . sqlSelect "documents" $ do
      let results =
            mapM_ sqlResult
              $ "documents.id"
            -- Include sort expressions as DISTINCT demands it.
              : map (\dobr -> dobrExpr dobr <+> "AS" <+> dobrName dobr) orderList
      when (documentDomainNeedsDistinct docDomain) $ do
        sqlDistinct
      case documentDomainToSQL docDomain of
        [] -> unexpectedError "selectDocuments: empty domain"
        ((mainFilterMap, mainDomain) : rest) -> do
          mainDomain
          results
          mapM_ documentFilterToSQL . catMaybes $ map mainFilterMap docFilters
          sqlUnion $ for rest $ \(filterMap, domain) -> do
            toSQLCommand . sqlSelect "documents" $ do
              domain
              results
              mapM_ documentFilterToSQL . catMaybes $ map filterMap docFilters
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
    orderList      = map documentOrderByAscDescToSQL orders

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentByDocumentID Document where
  -- FIXME: Use domains/filters.
  query (GetDocumentByDocumentID did) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereDocumentWasNotPurged

data GetDocumentByShortDocumentID = GetDocumentByShortDocumentID DocumentID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetDocumentByShortDocumentID Document where
  query (GetDocumentByShortDocumentID shortDid) = do
    now <- currentTime
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereShortDocumentIDIs now shortDid

-- Like GetDocumentByDocumentID, but also retrieves purged docs,
-- only to be used for Dave
data GetDocumentForDave = GetDocumentForDave DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentForDave Document where
  -- FIXME: Use domains/filters.
  query (GetDocumentForDave did) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did

data GetDocumentAPICallbackResult = GetDocumentAPICallbackResult DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentAPICallbackResult (Maybe String) where
  query (GetDocumentAPICallbackResult did) = do
    runQuery_ . sqlSelect "api_callback_result" $ do
      sqlResult "callback_result"
      sqlWhereEq "document_id" did
      sqlLimit 1
    fetchMaybe runIdentity

data GetDocumentBySignatoryLinkID = GetDocumentBySignatoryLinkID SignatoryLinkID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentBySignatoryLinkID Document where
  -- FIXME: Use domains/filters.
  query (GetDocumentBySignatoryLinkID slid) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereEqSql "documents.id"
        . parenthesize
        . toSQLCommand
        . sqlSelect "signatory_links"
        $ do
            sqlResult "signatory_links.document_id"
            sqlWhereEq "signatory_links.id" slid
      sqlWhereDocumentWasNotPurged

data GetDocumentsBySignatoryLinkIDs = GetDocumentsBySignatoryLinkIDs [SignatoryLinkID]
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentsBySignatoryLinkIDs [Document] where
  -- FIXME: Use domains/filters.
  query (GetDocumentsBySignatoryLinkIDs slids) = do
    runQuery_ . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhereIn "signatory_links.id" slids
        sqlWhere "signatory_links.document_id = documents.id"
      sqlWhereDocumentWasNotPurged
    fetchMany toComposite

data GetDocumentByDocumentIDSignatoryLinkID = GetDocumentByDocumentIDSignatoryLinkID DocumentID SignatoryLinkID
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBQuery m GetDocumentByDocumentIDSignatoryLinkID Document where
  query (GetDocumentByDocumentIDSignatoryLinkID did slid) = do
    doc <- kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhere "signatory_links.document_id = documents.id"
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereSignatoryLinkIsNotForwaded
      sqlWhereDocumentWasNotPurged
    return doc

data GetDocumentByDocumentIDSignatoryLinkIDMagicHash = GetDocumentByDocumentIDSignatoryLinkIDMagicHash DocumentID SignatoryLinkID MagicHash
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBQuery m GetDocumentByDocumentIDSignatoryLinkIDMagicHash Document where
  query (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) = do
    now <- currentTime
    runQuery_ $ sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhere "signatory_links.document_id = documents.id"
        -- Thought for later: Here we might actually check if
        -- visibility rules allow a person to see this document, for
        -- example if sign order allows to see the document. For now
        -- we are sloppy and let a person see the document.
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereSomeSignatoryAccessTokenHasMagicHash mh
        sqlWhereSignatoryLinkIsNotForwaded
      sqlWhereDocumentWasNotPurged
    mdoc <- fetchMaybe toComposite
    case mdoc of
      Nothing  -> throwM $ SomeDBExtraException SignatoryTokenDoesNotMatch
      Just doc -> do
        let Just sl = getSigLinkFor slid doc
        unless (isValidSignatoryMagicHash mh now (documentstatus doc) sl) $ do
          throwM $ SomeDBExtraException SignatoryTokenDoesNotMatch
        return doc

-- CheckIfMagicHashIsValid will NOT throw exception and can be used to handle failures nicely
data CheckIfMagicHashIsValid = CheckIfMagicHashIsValid DocumentID SignatoryLinkID MagicHash
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBQuery m CheckIfMagicHashIsValid Bool where
  query (CheckIfMagicHashIsValid did slid mh) = do
    runQuery_ $ sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereExists $ sqlSelect "signatory_links" $ do
        sqlWhere "signatory_links.document_id = documents.id"
        sqlWhereSignatoryLinkIDIs slid
        sqlWhereSomeSignatoryAccessTokenHasMagicHash mh
        sqlWhereSignatoryLinkIsNotForwaded
      sqlWhereDocumentWasNotPurged
    mdoc <- fetchMaybe toComposite
    case (mdoc, join $ getSigLinkFor slid <$> mdoc) of
      (Just doc, Just sl) -> do
        now <- currentTime
        return $ isValidSignatoryMagicHash mh now (documentstatus doc) sl
      _ -> return False

data GetDocumentByDocumentIDAndShareableLinkHash = GetDocumentByDocumentIDAndShareableLinkHash DocumentID MagicHash
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentByDocumentIDAndShareableLinkHash Document where
  query (GetDocumentByDocumentIDAndShareableLinkHash did mh) = do
    kRunAndFetch1OrThrowWhyNot toComposite . sqlSelect "documents" $ do
      mapM_ sqlResult documentsSelectors
      sqlWhereDocumentIDIs did
      sqlWhereDocumentWasNotPurged
      sqlWhereEq "shareable_link_hash" mh

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
    runQuery_ . selectDocuments domain filters [] 1 $ do
      mapM_ sqlResult documentsSelectors
    fetchOne toComposite

data GetDocumentsWithSoftLimit = GetDocumentsWithSoftLimit DocumentDomain [DocumentFilter] [AscDesc DocumentOrderBy] (Int, Int, Int)
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentsWithSoftLimit (Int, [Document]) where
  query (GetDocumentsWithSoftLimit domain filters orders (offset, limit, soft_limit)) =
    do
      --analysis <- explainAnalyze $ toSQLCommand sql
      --trace ("ANALYSIS:" <+> analysis) $ return ()
      --trace (show $ toSQLCommand sql) $ return ()
      runQuery_ sql
      (count :: Int64, CompositeArray1 documents) <- fetchOne identity
      return (fromIntegral count, documents)
    where
      sql = sqlSelect "" $ do
        -- get ids of documents for total count
        sqlWith "selected_ids"
          . selectDocuments domain filters orders (offset + limit)
          $ do
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
        sqlResult
          $   "ARRAY(SELECT ("
          <>  mintercalate ", " documentsSelectors
          <>  ")::"
          <>  raw (ctName ctDocument)
          <+> "FROM relevant_ids ids JOIN documents USING (id) ORDER BY ids.position) AS documents"

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
      sqlWhere $ "timeout_time IS NOT NULL AND timeout_time <" <?> mtime
      sqlLimit size
    fetchMany toComposite

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

data GetRandomSignatoryLinkIDThatSignedRecently = GetRandomSignatoryLinkIDThatSignedRecently UTCTime
instance (MonadDB m, MonadThrow m) => DBQuery m GetRandomSignatoryLinkIDThatSignedRecently (Maybe SignatoryLinkID) where
  query (GetRandomSignatoryLinkIDThatSignedRecently time) = do
    runQuery_ . sqlSelect "signatory_links" $ do
      sqlJoinOn "documents" "signatory_links.document_id = documents.id"
      sqlResult "signatory_links.id"
      sqlWhere $ "signatory_links.sign_time >" <?> time
      sqlWhereDocumentWasNotPurged
      sqlOrderBy "random()"
      sqlLimit 1
    fetchMaybe runIdentity
