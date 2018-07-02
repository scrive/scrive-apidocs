module Doc.API.V2.Calls.DocumentGetCalls (
  docApiV2List
, docApiV2Get
, docApiV2GetByShortID
, docApiV2GetQRCode
, docApiV2History
, docApiV2EvidenceAttachments
, docApiV2FilesMain
, docApiV2FilesPage
, docApiV2FilesPagesCount
, docApiV2FilesGet
, docApiV2Texts
) where

import Control.Monad.Base
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Time
import Data.Unjson
import Happstack.Server.Types
import Log
import Text.JSON.Types (JSValue(..))
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.Text as T
import qualified Text.JSON as J

import API.V2
import API.V2.Parameters
import AppView (respondWithPDF)
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.List
import Doc.API.V2.JSON.Misc (evidenceAttachmentsToJSONBS)
import Doc.Data.MainFile
import Doc.DocSeal (presealDocumentFile)
import Doc.DocStateData
import Doc.DocStateQuery (getDocByDocIDAndAccessTokenV2)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils (fileFromMainFile)
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Texts
import EvidenceLog.Model
import EvidenceLog.View
import File.Model
import File.Storage
import Kontra
import KontraLink
import Log.Identifier
import MagicHash
import OAuth.Model
import User.Model
import Util.PDFUtil
import Util.QRCode (encodeQR)
import Util.SignatoryLinkUtils
import qualified Doc.EvidenceAttachments as EvidenceAttachments

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  -- Permissions
  (user, _) <- getAPIUserWithPad APIDocCheck
  -- Parameters
  offset   <- apiV2ParameterDefault 0   (ApiV2ParameterInt  "offset")
  maxcount <- apiV2ParameterDefault 100 (ApiV2ParameterInt  "max")
  filters  <- apiV2ParameterDefault []  (ApiV2ParameterJSON "filter" unjsonDef)
  sorting  <- apiV2ParameterDefault defaultDocumentAPISort (ApiV2ParameterJSON "sorting" unjsonDef)
  -- API call actions
  let documentFilters = (DocumentFilterUnsavedDraft False):(join $ toDocumentFilter (userid user) <$> filters)
  let documentSorting = (toDocumentSorting <$> sorting)
  logInfo "Fetching list of documents from the database" $ object [
      identifier_ $ userid user
    , "offset"    .= offset
    , "max_count" .= maxcount
    , "filters"   .= map show documentFilters
    , "sorting"   .= map show documentSorting
    ]
  startQueryTime <- liftBase getCurrentTime
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit (DocumentsVisibleToUser $ userid user) documentFilters documentSorting (offset, 1000, maxcount)
  finishQueryTime <- liftBase getCurrentTime
  logInfo "Fetching for docApiV2List done" $ object [
      "query_time" .= (realToFrac $ diffUTCTime finishQueryTime startQueryTime :: Double)
    ]
  -- Result
  let headers = mkHeaders [("Content-Type","application/json; charset=UTF-8")]
  return $ Ok $ Response 200 headers nullRsFlags (listToJSONBS (allDocsCount,(\d -> (documentAccessForUser user d,d)) <$> allDocs)) Nothing

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = logDocument did . api $ do
  mslid <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  doc <- dbQuery $ GetDocumentByDocumentID did
  da <- guardDocumentReadAccess mslid doc
  return $ Ok (unjsonDocument da, doc)

docApiV2GetByShortID :: Kontrakcja m => DocumentID -> m Response
docApiV2GetByShortID shortDid = api $ do
  logInfo "docApiV2GetByShortID" $ object $ [ identifier ("short_"<>) shortDid ]
  when (length (show shortDid) > 6) $
    apiError $ requestParameterInvalid "short_document_id"
    "was greater than 6 digits"
  doc <- dbQuery $ GetDocumentByShortDocumentID shortDid
  da <- guardDocumentReadAccess Nothing doc
  logInfo "docApiV2GetByShortID: got a document" $ logObject_ doc
  guardDocumentStatus Pending doc
  return $ Ok (unjsonDocument da, doc)

-- | Produces a QR-encoded .png image that contains a URL of form
-- 'scrive://$domain/s/$did/$signatorylinkid/$signatorymagichash'.
docApiV2GetQRCode :: forall m . Kontrakcja m => DocumentID -> SignatoryLinkID
                  -> m Response
docApiV2GetQRCode did slid = logDocument did . logSignatory slid . api $ do
  doc <- dbQuery $ GetDocumentByDocumentID did

  void $ guardDocumentReadAccess Nothing doc
  guardDocumentStatus Pending doc

  domainURL <- ctxDomainUrl <$> getContext
  sigLink   <- apiGuardJust (signatoryLinkForDocumentNotFound did slid) $
               getSigLinkFor slid doc
  qrCode    <- liftIO $ encodeQR (mkSignLink domainURL sigLink)

  return $ Ok qrCode

    where
      -- | Create a URL to be QR-encoded.
      mkSignLink :: String -> SignatoryLink -> String
      mkSignLink domainURL sigLink =
        let relativeLink = LinkSignDoc did sigLink
        in (setProtocol domainURL) <> (show relativeLink)

      -- | Sets the protocol part of the URL to 'scrive://'.
      setProtocol :: String -> String
      setProtocol url = case break (== ':') url of
        (_protocol, srv@(':':'/':'/':_)) ->
          "scrive" ++ srv                -- http://scrive.com
        (_srv,      _null_or_port      ) ->
          "scrive://" ++ url             -- scrive.com or localhost:8000

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History did = logDocument did . api $ do
  -- Permissions
  (user,_) <- getAPIUser APIDocCheck
  doc <- dbQuery $ GetDocumentByDocumentID did
  guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc
  -- Parameters
  mLangCode <- apiV2ParameterOptional (ApiV2ParameterText "lang")
  mLang <- case fmap (langFromCode . T.unpack) mLangCode of
    Nothing -> return Nothing
    Just Nothing -> do
      apiError $ requestParameterInvalid "lang" "Not a valid or supported language code"
    Just (Just l) -> return $ Just l
  -- API call actions
  switchLang $ fromMaybe (lang $ usersettings user) mLang
  evidenceLog <- dbQuery $ GetEvidenceLog did
  events <- reverse <$> eventsJSListFromEvidenceLog doc evidenceLog
  -- Result
  return $ Ok $ JSObject (J.toJSObject $ [("events", JSArray events)])

docApiV2EvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2EvidenceAttachments did = logDocument did . api $ withDocumentID did $ do
  (user,_) <- getAPIUser APIDocCheck
  doc <- theDocument
  guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc
  eas <- EvidenceAttachments.extractAttachmentsList doc
  let headers = mkHeaders [("Content-Type","application/json; charset=UTF-8")]
  return $ Ok $ Response 200 headers nullRsFlags (evidenceAttachmentsToJSONBS (documentid doc) eas) Nothing

docApiV2FilesMain :: Kontrakcja m => DocumentID -> String -> m Response
docApiV2FilesMain did _filenameForBrowser = logDocument did . api . withDocAccess did $ \doc -> do
  download     <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  case documentstatus doc of
    Closed ->
      fileFromMainFile (documentsealedfile doc)
        >>= maybe errorNonexistingSealedMainFile return
        >>= getFileContents
        >>= return . Ok . respondWithPDF download
    _      ->
      fileFromMainFile (documentfile doc)
        >>= maybe errorNonexistingMainFile return
        >>= presealDocumentFile doc
        >>= either errorPresealDocumentFile return
        >>= return . Ok . respondWithPDF download
  where
    errorNonexistingSealedMainFile = apiError $ documentStateErrorWithCode 503 "The sealed PDF for the document is not ready yet, please wait and try again."
    errorNonexistingMainFile = apiError $ resourceNotFound "The document has no main file"
    errorPresealDocumentFile err = apiError . serverError . T.pack $ err

getDocBySignatoryLinkIdOrAccessToken :: Kontrakcja m =>
  DocumentID -> Maybe SignatoryLinkID -> Maybe MagicHash -> m Document
getDocBySignatoryLinkIdOrAccessToken did mslid maccesstoken =
  withDocumentID did $ case maccesstoken of
    Just token -> getDocByDocIDAndAccessTokenV2 did token
    Nothing    -> do
      void $ guardDocumentReadAccess mslid =<< theDocument
      theDocument

withDocAccess :: Kontrakcja m => DocumentID -> (Document -> m a) -> m a
withDocAccess did dochandler = do
  mslid        <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  maccesstoken <- apiV2ParameterOptional (ApiV2ParameterRead "access_token")
  doc          <- getDocBySignatoryLinkIdOrAccessToken did mslid maccesstoken

  whenJust mslid $ \slid -> do
    guardThatDocumentIsReadableBySignatories doc
    guardSignatoryNeedsToIdentifyToView slid doc
  dochandler doc

withDocFileAccess :: Kontrakcja m => DocumentID -> FileID -> m a -> m a
withDocFileAccess did fid action = withDocAccess did $ \_doc -> do
  filenotindocument <- not <$> (dbQuery $ FileInDocument did fid)
  if filenotindocument
    then apiError $ resourceNotFound "No file with given fileid associated with document"
    else action

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
docApiV2FilesGet did fid filename = logDocumentAndFile did fid . api . withDocFileAccess did fid $ do
  download     <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  fileContents <- getFileIDContents fid
  let filename' = map toLower filename
      contentType | isSuffixOf ".pdf" filename' = "application/pdf"
                  | isSuffixOf ".png" filename' = "image/png"
                  | isSuffixOf ".jpg" filename' = "image/jpeg"
                  | otherwise = "application/octet-stream"
      additionalDownloadHeader = if (download) then [("Content-Disposition", "attachment")] else []
      headers = mkHeaders $ [("Content-Type", contentType)] ++ additionalDownloadHeader
  return $ Ok $ Response 200 headers nullRsFlags (BSL.fromStrict fileContents) Nothing

-- We return 503 if sealed file is still pending, else we return JSON with number of pages
docApiV2FilesPagesCount :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2FilesPagesCount did fid = logDocument did . api . withDocFileAccess did fid $ do
  getFileIDContents fid
    >>= liftIO . getNumberOfPDFPages
    >>= either errorCountingPages return
    >>= return . Ok . pageCountJSON
  where
    pageCountJSON pagecount = JSObject . J.toJSObject $ [("pages", JSRational False (toRational pagecount))]
    errorCountingPages _err = do
      let msg = "Counting number of pages failed"
      logAttention_ msg
      apiError $ serverError msg

-- We return 503 if sealed file is still pending, else we return PNG of the page
docApiV2FilesPage :: Kontrakcja m => DocumentID -> FileID -> Int -> m Response
docApiV2FilesPage did fid pagenumber = logDocument did . api . withDocFileAccess did fid $ do
  pixelwidth <- apiV2ParameterObligatory (ApiV2ParameterRead "pixelwidth")
  getFileIDContents fid
    >>= (\content -> renderPage content pagenumber $ clamp pixelwidth)
    >>= maybe errorRenderingFailed return
    >>= \bytes -> return . Ok $ Response 200 headers nullRsFlags (BSL.fromStrict bytes) Nothing
  where
    clamp = min 2000 . max 100
    errorRenderingFailed = do
      logAttention "Rendering PDF page failed" $ object [ "page" .= show pagenumber]
      apiError $ serverError "Rendering PDF page failed"
    headers = mkHeaders [("Cache-Control", "max-age=604800"), ("Content-Type", "image/png")]

-------------------------------------------------------------------------------

docApiV2Texts :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2Texts did fid = logDocumentAndFile did fid . api $ do
  -- Permissions
  (user,_) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardDocumentStatus Preparation =<< theDocument
    -- Parameters
    -- We have a "black-box" JSON structure here, see Doc.Texts for details
    -- If you feel motivated you can refactor this to proper data type with
    -- Unjson instance to make things better :)
    jsonText <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "json")
    (json :: JSValue) <- case J.decode jsonText of
            J.Ok j -> return j
            _ -> apiError $ requestParameterParseError "json" "Could not read JSON"
    -- API call actions
    doc <- theDocument
    case mainfileid <$> documentfile doc of
      Nothing -> apiError $ resourceNotFound "The document has no main file"
      Just mainFid -> when (fid /= mainFid) (apiError $ resourceNotFound "Given 'fileid' is not the main file of the document")
    content <- getFileIDContents fid
    eitherResult <- runJavaTextExtract json content
    case eitherResult of
      Left err -> apiError $ serverError err
      -- Return
      Right res -> return $ Ok res
