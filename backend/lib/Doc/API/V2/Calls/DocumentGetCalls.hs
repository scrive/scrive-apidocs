module Doc.API.V2.Calls.DocumentGetCalls (
  docApiV2List
, docApiV2Get
, docApiV2GetByShortID
, docApiV2GetQRCode
, docApiV2History
, docApiV2EvidenceAttachments
, docApiV2FilesMain
, docApiV2FilesEvidence
, docApiV2FilesFull
, docApiV2FilesPage
, docApiV2FilesPagesCount
, docApiV2FilesGet
, docApiV2SigningData
, docApiV2CanBeStarted
-- * Functions for tests
, docApiV2FilesFullForTests
) where

import Codec.Archive.Zip
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.List.Extra (nubOrd, nubOrdOn)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Unjson
import Happstack.Server.Types
import Log
import System.FilePath ((<.>), takeExtension)
import Text.JSON.Types (JSValue(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Text.JSON as J

import AccessControl.Check
import AccessControl.Model
import AccessControl.Types
import API.V2
import API.V2.Parameters
import API.V2.Utils
import AppView (respondWithPDF, respondWithZipFile)
import DB
import Doc.AccessControl
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.List
import Doc.API.V2.JSON.Misc (evidenceAttachmentsToJSONBS)
import Doc.API.V2.JSON.SigningData
import Doc.DocInfo
import Doc.DocSeal (presealDocumentFile)
import Doc.DocStateData
import Doc.DocStateQuery (getDocByDocIDAndAccessTokenV2)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Types.SignatoryAccessToken
import Doc.Types.SigningData
import Doc.Validation.Starting (validateDocumentForStarting)
import EID.Signature.Model
import EvidenceLog.Model
import EvidenceLog.View
import File.Model
import File.Storage
import File.Types
import Kontra
import KontraLink
import Log.Identifier
import Log.Utils
import MagicHash
import OAuth.Model
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.PDFUtil
import Util.QRCode (encodeQR)
import Util.SignatoryLinkUtils
import qualified Doc.EvidenceAttachments as EvidenceAttachments

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  -- Permissions
  (user, _) <- getAPIUserWithPad APIDocCheck
  -- Parameters
  offset    <- apiV2ParameterDefault 0 (ApiV2ParameterInt "offset")
  maxcount  <- apiV2ParameterDefault 100 (ApiV2ParameterInt "max")
  filters   <- apiV2ParameterDefault [] (ApiV2ParameterJSON "filter" unjsonDef)
  sorting   <- apiV2ParameterDefault defaultDocumentAPISort
                                     (ApiV2ParameterJSON "sorting" unjsonDef)
  -- API call actions
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
  let documentFilters =
        DocumentFilterUnsavedDraft False : (toDocumentFilter (user ^. #id) =<< filters)
      documentSorting    = toDocumentSorting <$> sorting
      useFolderListCalls = ugwpSettings ugwp ^. #useFolderListCalls
  logInfo "Fetching list of documents from the database" $ object
    [ identifier $ user ^. #id
    , "offset" .= offset
    , "max_count" .= maxcount
    , "filters" .= map showt documentFilters
    , "sorting" .= map showt documentSorting
    , "use_folder_list_calls" .= useFolderListCalls
    ]
  if useFolderListCalls
    then do {- new style -}
      -- we don't care if the role is explicit (set) or implicit (derived from
      -- some user or user group property), hence `nubBy`
      allUserRoles <- nubOrdOn accessRoleTarget
        <$> dbQuery (GetRolesIncludingInherited user $ ugwpUG ugwp)
      let allPerms =
            nubOrd . concatMap hasPermissions $ map accessRoleTarget allUserRoles
          fullReadFids = (`mapMaybe` allPerms) $ \case
            Permission PermCanDo ReadA (DocumentInFolderR fid) -> Just fid
            _ -> Nothing
          sharedFids = (`mapMaybe` allPerms) $ \case
            Permission PermCanDo ReadA (DocumentInFolderR fid) -> Just fid
            Permission PermCanDo ReadA (SharedTemplateR fid) -> Just fid
            _ -> Nothing
          startedFids = (`mapMaybe` allPerms) $ \case
            Permission PermCanDo ReadA (DocumentInFolderR fid) -> Just fid
            Permission PermCanDo ReadA (DocumentAfterPreparationR fid) -> Just fid
            _ -> Nothing
      ((allDocsCount, allDocs), time) <- timed . dbQuery $ GetDocumentsWithSoftLimit
        (DocumentsVisibleToSigningPartyOrByFolders (user ^. #id)
                                                   sharedFids
                                                   startedFids
                                                   fullReadFids
        )
        documentFilters
        documentSorting
        (offset, 1000, maxcount)
      logInfo "Fetching for docApiV2List done using folders" $ object
        [ "query_time" .= time
        , identifier $ user ^. #id
        , identifier $ user ^. #groupID
        , "folder_ids_shared" .= map show sharedFids
        , "folder_ids_started" .= map show startedFids
        , "folder_ids_full_read" .= map show fullReadFids
        ]
      let headers = mkHeaders [("Content-Type", "application/json; charset=UTF-8")]
      return . Ok $ Response
        200
        headers
        nullRsFlags
        (listToJSONBS
          ( allDocsCount
          , (\d -> (documentAccessByFolder user d allUserRoles, d)) <$> allDocs
          )
        )
        Nothing
    else do {- old style -}
      ((allDocsCount, allDocs), time) <- timed . dbQuery $ GetDocumentsWithSoftLimit
        (DocumentsVisibleToUser $ user ^. #id)
        documentFilters
        documentSorting
        (offset, 1000, maxcount)
      logInfo "Fetching for docApiV2List done" $ object
        ["query_time" .= time, identifier $ user ^. #id, identifier $ user ^. #groupID]
      -- Result
      let headers = mkHeaders [("Content-Type", "application/json; charset=UTF-8")]
      return . Ok $ Response
        200
        headers
        nullRsFlags
        (listToJSONBS
          (allDocsCount, (\d -> (documentAccessForUser user d, d)) <$> allDocs)
        )
        Nothing

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = logDocument did . api $ do
  mslid <- apiV2ParameterOptional (ApiV2ParameterRead "signatory_id")
  doc   <- dbQuery $ GetDocumentByDocumentID did
  da    <- guardDocumentReadAccess mslid doc
  return $ Ok (unjsonDocument da, doc)

docApiV2GetByShortID :: Kontrakcja m => DocumentID -> m Response
docApiV2GetByShortID shortDid = api $ do
  logInfo "docApiV2GetByShortID" $ object [identifierMapLabel ("short_" <>) shortDid]
  when (length (show shortDid) > 6) . apiError $ requestParameterInvalid
    "short_document_id"
    "was greater than 6 digits"
  doc <- dbQuery $ GetDocumentByShortDocumentID shortDid
  da  <- guardDocumentReadAccess Nothing doc
  logInfo "docApiV2GetByShortID: got a document" $ logObject_ doc
  guardDocumentStatus Pending doc
  return $ Ok (unjsonDocument da, doc)

-- | Produces a QR-encoded .png image that contains a URL of form
-- 'scrive://$domain/s/$did/$signatorylinkid/$signatorymagichash'.
docApiV2GetQRCode :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2GetQRCode did slid = logDocument did . logSignatory slid . api $ do
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkID did slid

  void $ guardDocumentReadAccess Nothing doc
  guardDocumentStatus Pending doc

  domainURL <- view (#brandedDomain % #url) <$> getContext
  sigLink   <- apiGuardJust (signatoryLinkForDocumentNotFound did slid)
    $ getSigLinkFor slid doc
  magicHash <-
    let msat = find ((== SignatoryAccessTokenForQRCode) . signatoryAccessTokenReason)
                    (signatoryaccesstokens sigLink)
    in  case msat of
          Just sat -> return $ signatoryAccessTokenHash sat
          Nothing  -> do
            dbUpdate $ NewSignatoryAccessToken slid SignatoryAccessTokenForQRCode Nothing
  qrCode <- liftIO . encodeQR $ T.unpack (mkSignLink domainURL magicHash)
  return $ Ok qrCode

  where
    -- | Create a URL to be QR-encoded.
    mkSignLink :: Text -> MagicHash -> Text
    mkSignLink domainURL mh =
      let relativeLink = LinkSignDocMagicHash did slid mh
      in  T.pack $ setProtocol (T.unpack domainURL) <> show relativeLink

    -- | Sets the protocol part of the URL to 'scrive://'.
    setProtocol :: String -> String
    setProtocol url = case break (== ':') url of
      (_protocol, srv@(':' : '/' : '/' : _)) -> "scrive" <> srv                -- http://scrive.com
      (_srv     , _null_or_port            ) -> "scrive://" <> url             -- scrive.com or localhost:8000

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History did = logDocument did . api $ do
  -- Permissions
  (user, _)    <- getAPIUser APIDocCheck
  doc          <- dbQuery $ GetDocumentByDocumentID did
  requiredPerm <- apiRequireDocPermission ReadA doc
  apiAccessControlWithError user requiredPerm (apiError documentActionForbidden) $ do
    -- Parameters
    mLangCode <- apiV2ParameterOptional (ApiV2ParameterText "lang")
    mLang     <- case fmap langFromCode mLangCode of
      Nothing      -> return Nothing
      Just Nothing -> do
        apiError $ requestParameterInvalid "lang" "Not a valid or supported language code"
      Just (Just l) -> return $ Just l
    -- API call actions
    switchLang $ fromMaybe (user ^. #settings % #lang) mLang
    evidenceLog <- dbQuery $ GetEvidenceLog did
    events      <- reverse <$> eventsJSListFromEvidenceLog doc evidenceLog
    -- Result
    return . Ok $ JSObject (J.toJSObject [("events", JSArray events)])

docApiV2EvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2EvidenceAttachments did = logDocument did . api . withDocumentID did $ do
  (user, _)    <- getAPIUser APIDocCheck
  doc          <- theDocument
  requiredPerm <- apiRequireDocPermission ReadA doc
  apiAccessControlWithError user requiredPerm (apiError documentActionForbidden) $ do
    eas <- EvidenceAttachments.extractAttachmentsList doc
    let headers = mkHeaders [("Content-Type", "application/json; charset=UTF-8")]
    return . Ok $ Response 200
                           headers
                           nullRsFlags
                           (evidenceAttachmentsToJSONBS (documentid doc) eas)
                           Nothing

docApiV2FilesMain :: forall  m . Kontrakcja m => DocumentID -> m Response
docApiV2FilesMain did = logDocument did . api . withDocAccess did $ \doc -> do
  download <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  if isClosed doc
    then fmap (Ok . respondWithPDF download) . getFileContents =<< maybe
      errorNonexistingSealedMainFile
      return
      (documentclosedmainfile doc)
    else
      fmap (Ok . respondWithPDF download)
      .   either errorPresealDocumentFile return
      =<< presealDocumentFile doc
      =<< maybe errorNonexistingMainFile return (documentinputfile doc)

  where
    errorNonexistingSealedMainFile = apiError $ documentStateErrorWithCode
      503
      "The sealed PDF for the document is not ready yet, please wait and try again."
    errorNonexistingMainFile =
      apiError $ resourceNotFound "The document has no main file"

    errorPresealDocumentFile :: forall a . Text -> m a
    errorPresealDocumentFile = apiError . serverError

docApiV2FilesEvidence :: Kontrakcja m => DocumentID -> MagicHash -> m Response
docApiV2FilesEvidence did secret = logDocument did . api . withDocumentID did $ do
  download <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
  doc      <- theDocument
  when (documentevidencefilesecret doc /= Just secret) $ apiError documentActionForbidden

  case documentfile doc of
    Just ClosedVerimiQesFile {..} -> do
      contents <- getFileContents $ digitallySignedFile evidenceFile
      return . Ok $ respondWithPDF download contents
    _ -> do
      logAttention_
        "docApiV2FilesEvidence with valid secret, but evidence file does not exist"
      apiError $ documentStateErrorWithCode 503 "Evidence file does not exist."

docApiV2SigningData :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigningData did slid = logDocument did . logSignatory slid . api $ do
  user <- getAPIUserWithAPIPersonal
  doc  <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkID did slid
  sl   <-
    apiGuardJust (signatoryLinkForDocumentNotFound (documentid doc) slid)
    . getSigLinkFor slid
    $ doc
  requiredPerm <- apiRequireDocPermission ReadA doc
  apiAccessControl user requiredPerm $ do
    ssdData <- dbQuery (GetESignature slid) >>= \case
      Nothing   -> return . Left $ signatorylinkauthenticationtosignmethod sl
      Just esig -> return $ Right esig
    let ssdHasSigned = isSignatoryAndHasSigned sl
    return . Ok . ssdToJson (signatorylinkhidepn sl) sl $ SignatorySigningData { .. }

getDocBySignatoryLinkIdOrAccessToken
  :: Kontrakcja m => DocumentID -> Maybe SignatoryLinkID -> Maybe MagicHash -> m Document
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
    case getSigLinkFor slid doc of
      Just sl | signatoryisauthor sl -> return ()
      _ -> guardThatDocumentIsReadableBySignatories doc
    guardSignatoryNeedsToIdentifyToView slid doc
  dochandler doc

withDocFileAccess :: Kontrakcja m => DocumentID -> FileID -> m a -> m a
withDocFileAccess did fid action = withDocAccess did $ \_doc -> do
  filenotindocument <- not <$> dbQuery (FileInDocument did fid)
  if filenotindocument
    then apiError $ resourceNotFound "No file with given fileid associated with document"
    else action

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> Maybe Text -> m Response
docApiV2FilesGet did fid mFilename =
  logDocumentAndFile did fid . api . withDocFileAccess did fid $ do
    download     <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")
    file         <- dbQuery $ GetFileByFileID fid
    fileContents <- getFileContents file
    let filename' = T.toLower $ fromMaybe (filename file) mFilename
        contentType | T.isSuffixOf ".pdf" filename' = "application/pdf"
                    | T.isSuffixOf ".png" filename' = "image/png"
                    | T.isSuffixOf ".jpg" filename' = "image/jpeg"
                    | otherwise                     = "application/octet-stream"
        additionalDownloadHeader =
          if download then [("Content-Disposition", "attachment")] else []
        headers = mkHeaders $ [("Content-Type", contentType)] <> additionalDownloadHeader
    return . Ok $ Response 200 headers nullRsFlags (BSL.fromStrict fileContents) Nothing

-- We return 503 if sealed file is still pending, else we return JSON with number of pages
docApiV2FilesPagesCount :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2FilesPagesCount did fid = logDocument did . api . withDocFileAccess did fid $ do
  fmap (Ok . pageCountJSON)
    .   either errorCountingPages return
    =<< (liftIO . getNumberOfPDFPages)
    =<< getFileIDContents fid

  where
    pageCountJSON pagecount =
      JSObject . J.toJSObject $ [("pages", JSRational False (toRational pagecount))]
    errorCountingPages _err = do
      let msg = "Counting number of pages failed"
      logAttention_ msg
      apiError $ serverError msg

-- We return 503 if sealed file is still pending, else we return PNG of the page
docApiV2FilesPage :: Kontrakcja m => DocumentID -> FileID -> Int -> m Response
docApiV2FilesPage did fid pagenumber =
  logDocument did . api . withDocFileAccess did fid $ do
    pixelwidth <- apiV2ParameterObligatory (ApiV2ParameterRead "pixelwidth")
    getFileIDContents fid
      >>= (\content -> renderPage content pagenumber $ clamp pixelwidth)
      >>= maybe errorRenderingFailed return
      >>= \bytes -> return . Ok $ Response 200
                                           headers
                                           nullRsFlags
                                           (BSL.fromStrict bytes)
                                           Nothing
  where
    clamp                = min 2000 . max 100
    errorRenderingFailed = do
      logAttention "Rendering PDF page failed" $ object ["page" .= show pagenumber]
      apiError $ serverError "Rendering PDF page failed"
    headers =
      mkHeaders [("Cache-Control", "max-age=604800"), ("Content-Type", "image/png")]

docApiV2FilesFull :: Kontrakcja m => DocumentID -> m Response
docApiV2FilesFull did = logDocument did . api . withDocAccess did $ \doc -> do
  download <- apiV2ParameterDefault False (ApiV2ParameterBool "as_download")

  files    <- docApiV2FilesFullInternal doc
  now      <- currentTime
  let timestamp = round $ utcTimeToPOSIXSeconds now
      contents  = BSL.toStrict . fromArchive $ foldr
        (\(n, c) -> addEntryToArchive (toEntry n timestamp (BSL.fromStrict c)))
        emptyArchive
        files

  return . Ok $ respondWithZipFile download contents

docApiV2FilesFullInternal :: Kontrakcja m => Document -> m [(FilePath, BS.ByteString)]
docApiV2FilesFullInternal doc = do
  documentFiles <- if isClosed doc
    then case documentfile doc of
      Just (ClosedFile DigitallySignedFile {..}) -> do
        contents <- getFileContents digitallySignedFile
        return [(mkFileName . T.unpack $ documenttitle doc, contents)]

      Just ClosedVerimiQesFile {..} -> do
        mainfileContents     <- getFileContents mainfileWithQesSignatures
        evidenceFileContents <- getFileContents $ digitallySignedFile evidenceFile
        return
          [ (mkFileName . T.unpack $ documenttitle doc, mainfileContents)
          , ( mkFileName . T.unpack $ documenttitle doc <> "_evidence"
            , evidenceFileContents
            )
          ]

      _ -> apiError $ documentStateErrorWithCode
        503
        "The sealed PDF for the document is not ready yet, please wait and\
              \ try again"
    else do
      inputFile <-
        whenNothing (documentinputfile doc) . apiError $ documentStateErrorWithCode
          503
          "The document has no main file"

      ePresealFile <- presealDocumentFile doc inputFile
      case ePresealFile of
        Left  err      -> apiError $ serverError err
        -- This replicates existing behaviour, which is inconsistent with the
        -- naming in mail attachments (there we use documenttitle for both
        -- closed and input files). See ticket CORE-2499.
        Right contents -> return [(T.unpack $ filename inputFile, contents)]

  let separateAuthorAttachments = filter
        (if isClosed doc then not . authorattachmentaddtosealedfile else const True)
        (documentauthorattachments doc)

      separateSignatoryAttachments
        | isClosed doc = []
        | otherwise    = concatMap signatoryattachments (documentsignatorylinks doc)

  authorAttachmentFiles <- forM separateAuthorAttachments $ \att -> do
    file     <- dbQuery . GetFileByFileID $ authorattachmentfileid att
    contents <- getFileContents file
    return (mkFileName . T.unpack $ authorattachmentname att, contents)

  signatoryAttachmentFiles <-
    fmap catMaybes . forM separateSignatoryAttachments $ \att -> do
      case signatoryattachmentfile att of
        Nothing  -> return Nothing
        Just fid -> do
          file     <- dbQuery $ GetFileByFileID fid
          contents <- getFileContents file
          return $ Just (mkFileName (T.unpack $ signatoryattachmentname att), contents)

  return $ documentFiles <> authorAttachmentFiles <> signatoryAttachmentFiles

  where
    mkFileName name | takeExtension name == ".pdf" = name
                    | otherwise                    = name <.> "pdf"

docApiV2FilesFullForTests :: Kontrakcja m => Document -> m [(FilePath, BS.ByteString)]
docApiV2FilesFullForTests = docApiV2FilesFullInternal

docApiV2CanBeStarted :: Kontrakcja m => DocumentID -> m Response
docApiV2CanBeStarted did = logDocument did . api $ do
  -- Permissions
  (user, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument
    Ok . toJSON . validateDocumentForStarting <$> theDocument
