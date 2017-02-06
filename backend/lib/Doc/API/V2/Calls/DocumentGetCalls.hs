module Doc.API.V2.Calls.DocumentGetCalls (
  docApiV2List
, docApiV2Get
, docApiV2History
, docApiV2EvidenceAttachments
, docApiV2FilesMain
, docApiV2FilesGet
, docApiV2Texts
) where

import Data.Char
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
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils (fileFromMainFile)
import Doc.Logging
import Doc.Model
import Doc.Texts
import EvidenceLog.Model
import EvidenceLog.View
import File.Model
import File.Storage
import Kontra
import KontraPrelude
import Log.Identifier
import OAuth.Model
import User.Model
import qualified Doc.EvidenceAttachments as EvidenceAttachments

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  -- Permissions
  (user, _) <- getAPIUserWithPad APIDocCheck
  -- Parameters
  offset   <- apiV2ParameterDefault 0   (ApiV2ParameterInt  "offset")
  maxcount <- apiV2ParameterDefault 100 (ApiV2ParameterInt  "max")
  filters  <- apiV2ParameterDefault []  (ApiV2ParameterJSON "filter" unjsonDef)
  sorting  <- apiV2ParameterDefault []  (ApiV2ParameterJSON "sorting" unjsonDef)
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
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit (DocumentsVisibleToUser $ userid user) documentFilters documentSorting (offset, 1000, maxcount)
  logInfo_ "Fetching done"
  -- Result
  let headers = mkHeaders [("Content-Type","application/json; charset=UTF-8")]
  return $ Ok $ Response 200 headers nullRsFlags (listToJSONBS (allDocsCount,(\d -> (documentAccessForUser user d,d)) <$> allDocs)) Nothing

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = logDocument did . api $ do
  mslid <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  doc <- dbQuery $ GetDocumentByDocumentID did
  da <- guardDocumentReadAccess mslid doc
  return $ Ok (unjsonDocument da, doc)

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History did = logDocument did . api $ do
  -- Permissions
  (user,_) <- getAPIUser APIDocCheck
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
  doc <- dbQuery $ GetDocumentByDocumentID did
  guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc
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
docApiV2FilesMain did _filenameForBrowser = logDocument did . api $ do
  mslid <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  download <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  fileContents <- withDocumentID did $ do
    doc <- theDocument
    _ <- guardDocumentReadAccess mslid doc
    when (isJust mslid) $ guardSignatoryNeedsToIdentifyToView (fromJust mslid) doc
    case documentstatus doc of
      Closed -> do
        mFile <- fileFromMainFile (documentsealedfile doc)
        case mFile of
          Nothing -> apiError $ documentStateErrorWithCode 503 "The sealed PDF for the document is not ready yet, please wait and try again."
          Just file -> getFileContents file
      _ -> do
        mFile <- fileFromMainFile (documentfile doc)
        case mFile of
          Nothing -> apiError $ resourceNotFound "The document has no main file"
          Just file -> do
           presealFile <- presealDocumentFile doc file
           case presealFile of
             Left err -> apiError $ serverError (T.pack err)
             Right f -> return $ f
  return $ Ok $ respondWithPDF download fileContents

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
docApiV2FilesGet did fid filename = logDocumentAndFile did fid . api $ do
  mslid <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  download <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  doc <- dbQuery $ GetDocumentByDocumentID did
  _ <- guardDocumentReadAccess mslid doc
  when (isJust mslid) $ guardSignatoryNeedsToIdentifyToView (fromJust mslid) doc
  let allfiles = maybeToList (mainfileid <$> documentfile doc) ++ maybeToList (mainfileid <$> documentsealedfile doc) ++
                      (authorattachmentfileid <$> documentauthorattachments doc) ++
                      (catMaybes $ map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc) ++
                      (catMaybes $ map fieldFileValue $ concatMap signatoryfields $ documentsignatorylinks doc) ++
                      (map highlightedPageFileID $ concatMap signatoryhighlightedpages $ documentsignatorylinks doc)
  if (all (/= fid) allfiles)
     then apiError $ resourceNotFound "No file with given fileid associated with document"
     else do
       fileContents <- getFileIDContents fid
       let filename' = map toLower filename
           contentType | isSuffixOf ".pdf" filename' = "application/pdf"
                       | isSuffixOf ".png" filename' = "image/png"
                       | isSuffixOf ".jpg" filename' = "image/jpeg"
                       | otherwise = "application/octet-stream"
           additionalDownloadHeader = if (download) then [("Content-Disposition", "attachment")] else []
           headers = mkHeaders $ [("Content-Type", contentType)] ++ additionalDownloadHeader
       return $ Ok $ Response 200 headers nullRsFlags (BSL.fromStrict fileContents) Nothing

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
