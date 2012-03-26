{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl(
    -- Exported utils or test functions
      sendReminderEmail
    -- Top level handlers
    , postDocumentChangeAction
    , handleDownloadFile
    , handleSignShow
    , handleSignShowOldRedirectToNew
    , signDocument
    , signDocumentIphoneCase
    , rejectDocument
    , rejectDocumentIphoneCase
    , handleAcceptAccountFromSign
    , handleSigAttach
    , handleDeleteSigAttach
    , handleAttachmentViewForViewer
    , handleShowUploadPage
    , handleAttachmentShare
    , handleAttachmentRename
    , handleCreateNewAttachment
    , handleTemplateShare
    , handleCreateNewTemplate
    , handleBulkOfferRemind
    , handleBulkOrderRemind
    , handleRubbishRestore
    , handleRubbishReallyDelete
    , handleIssueShowGet
    , handleIssueNewDocument
    , handleBulkContractRemind
    , handleIssueShowPost
    , jsonDocument
    , handleSaveDraft
    , handleSetAttachments
    , handleParseCSV
    , prepareEmailPreview
    , handleFileGet
    , handleAttachmentViewForAuthor
    , handleResend
    , handleChangeSignatoryEmail
    , handleRestart
    , handleCancel
    , showPage
    , showPreview
    , showPreviewForSignatory
    , handleCreateFromTemplate
    , handleFilePages
    , handlePageOfDocument
    , handlePageOfDocumentForSignatory
    , handleCSVLandpage
    , handleInvariantViolations
    , handleUpsalesDeleted

) where

import AppView
import DB.Classes
import DBError
import Doc.Action
import Doc.CSVUtils
import Doc.Model
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.DocStorage
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import InputValidation
import File.Model
import Kontra
import KontraError (internalError)
import KontraLink
import MagicHash (MagicHash)
import MinutesTime
import Misc
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Templates.Templates
import Templates.Trans
import Util.CSVUtil
import Util.FlashUtil
import Util.KontraLinkUtils
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils
import Doc.Invariants
import Stats.Control
import User.Utils
import API.Service.Model
import Company.Model

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Either
import Data.List
import Data.Maybe
import Happstack.Server.Types
import Happstack.Server hiding (simpleHTTP)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import Text.JSON hiding (Result)
import Text.JSON.Fields as JSON (json)
import qualified Text.JSON.Fields as JSON (field)
import Doc.DocDraft as Draft
import qualified User.Action
import Util.JSON
import qualified ELegitimation.BankID as BankID
import EvidenceLog.Model

{-
  Document state transitions are described in DocState.

  Here are all actions associated with transitions.
-}

{- |

    Handles an account setup from within the sign view.
-}
handleAcceptAccountFromSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
handleAcceptAccountFromSign documentid
                            signatorylinkid
                            magichash = do
  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  _ <- guardJustM $ User.Action.handleAccountSetupFromSign document signatorylink
  return $ LinkSignDoc document signatorylink

{- |
   Control the signing of a document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
   Method: POST
 -}
signDocumentIphoneCase :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
signDocumentIphoneCase did sid _ = signDocument did sid

signDocument :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m KontraLink
signDocument documentid
             signatorylinkid = do
  magichash <- guardJustM $ readField "magichash"
  fieldnames <- getAndConcat "fieldname"
  fieldvalues <- getAndConcat "fieldvalue"
  let fields = zip fieldnames fieldvalues
  mprovider <- readField "eleg"
  edoc <- case mprovider of
           Nothing -> Right <$> signDocumentWithEmail documentid signatorylinkid magichash fields
           Just provider -> do
               signature     <- getDataFnM $ look "signature"
               transactionid <- getDataFnM $ look "transactionid"
               esinfo <- BankID.verifySignatureAndGetSignInfo documentid signatorylinkid magichash provider signature transactionid
               case esinfo of
                    BankID.Problem msg -> return $ Left msg
                    BankID.Mismatch msg sfn sln spn -> do
                        document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
                        handleMismatch document signatorylinkid msg sfn sln spn
                        return $ Left msg
                    BankID.Sign sinfo ->  Right <$>  signDocumentWithEleg documentid signatorylinkid magichash fields sinfo
  case edoc of
    Right (Right (doc, olddoc)) -> do
      postDocumentChangeAction doc olddoc (Just signatorylinkid)
      udoc <- guardJustM $ runDBQuery $ GetDocumentByDocumentID documentid
      handleAfterSigning udoc signatorylinkid
    Right (Left (DBActionNotAvailable message)) -> do
      addFlash (OperationFailed, message)
      return LoopBack
    Right (Left (DBDatabaseNotAvailable message)) -> do
      addFlash (OperationFailed, message)
      return LoopBack
    Left msg -> do
      addFlash  (OperationFailed, msg)
      return LoopBack
    _ -> internalError
  where
    getAndConcat fname = getDataFnM (lookInputList fname) >>= return . map BSL.toString

handleMismatch :: Kontrakcja m => Document -> SignatoryLinkID -> String -> String -> String -> String -> m ()
handleMismatch doc sid msg sfn sln spn = do
        ctx <- getContext
        let Just sl = getSigLinkFor doc sid
        Log.eleg $ "Information from eleg did not match information stored for signatory in document." ++ show msg
        Right newdoc <- runDBUpdate $ CancelDocument (documentid doc) (ELegDataMismatch msg sid sfn sln spn)
                        (SignatoryActor (ctxtime ctx)
                         (ctxipnumber ctx)
                         (maybesignatory sl)
                         (getEmail sl)
                         sid)
        postDocumentChangeAction newdoc doc (Just sid)

{- |
    Call after signing in order to save the document for any user, and
    put up the appropriate modal.
-}
handleAfterSigning :: Kontrakcja m => Document -> SignatoryLinkID -> m KontraLink
handleAfterSigning document@Document{documentid} signatorylinkid = do
  ctx <- getContext
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  maybeuser <- runDBQuery $ GetUserByEmail (currentServiceID ctx) (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user-> do
      let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink) (getEmail signatorylink) (signatorylinkid)
      _ <- runDBUpdate $ SaveDocumentForUser documentid user signatorylinkid actor
      return ()
    _ -> return ()
  return $ LinkSignDoc document signatorylink


rejectDocumentIphoneCase :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
rejectDocumentIphoneCase did sid _ = rejectDocument did sid

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
 -}
rejectDocument :: Kontrakcja m
               => DocumentID
               -> SignatoryLinkID
               -> m KontraLink
rejectDocument documentid
               signatorylinkid1 = do
  magichash <- guardJustM $ readField "magichash"
  customtext <- getCustomTextField "customtext"

  edocs <- rejectDocumentWithChecks documentid signatorylinkid1 magichash customtext

  case edocs of
    Left (DBActionNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left _ -> internalError
    Right (document, olddocument) -> do
      postDocumentChangeAction document olddocument (Just signatorylinkid1)
      addFlashM $ modalRejectedView document
      return $ LoopBack

{- |
   Show the document to be signed
 -}

handleSignShowOldRedirectToNew :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m (Either KontraLink Response)
handleSignShowOldRedirectToNew did sid mh = do
  modifyContext (\ctx -> ctx { ctxmagichashes = Map.insert sid mh (ctxmagichashes ctx) })
  iphone <- isIphone
  if iphone -- For iphones we are returning full page due to cookie bug in mobile safari
    then Right <$> handleSignShow2 did sid
    else return $ Left $ LinkSignDocNoMagicHash did sid

handleSignShow :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Either KontraLink Response)
handleSignShow documentid
               signatorylinkid = do
  mmh <- readField "magichash"
  case mmh of
    Just mh -> do -- IMPORTANT!!! Keep this just for historical reasons
      modifyContext (\ctx -> ctx { ctxmagichashes = Map.insert signatorylinkid mh (ctxmagichashes ctx) })
      return $ Left (LinkSignDocNoMagicHash documentid signatorylinkid)
    Nothing -> Right <$> handleSignShow2 documentid signatorylinkid

handleSignShow2 :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
handleSignShow2 documentid
                signatorylinkid = do
  Context { ctxtime
          , ctxipnumber
          , ctxmagichashes } <- getContext
  let mmagichash = Map.lookup signatorylinkid ctxmagichashes

  magichash <- case mmagichash of
                 Just x -> return x
                 Nothing -> do
                   Log.debug $ "magichash for " ++ show documentid ++ "/" ++ show signatorylinkid ++ " not found"
                   internalError

  document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
  disableLocalSwitch
  switchLocale (getLocale document)
  invitedlink <- guardJust $ getSigLinkFor document signatorylinkid
  _ <- runDBUpdate $ MarkDocumentSeen documentid signatorylinkid magichash
       (SignatoryActor ctxtime ctxipnumber (maybesignatory invitedlink) (getEmail invitedlink) signatorylinkid)
  _ <- runDB $ addSignStatLinkEvent document invitedlink

  ctx <- getContext
  content <- pageDocumentSignView ctx document invitedlink
  simpleResponse content

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink String)
handleIssueShowGet docid = checkUserTOSGet $ do
  document <- guardRightM $ getDocByDocID docid
  disableLocalSwitch -- Don't show locale flag on this page
  switchLocale (getLocale document)
  user <- guardJustM $ ctxmaybeuser <$> getContext

  let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
  when (isAuthor (document, user) && isCanceled document && isJust mMismatchMessage) $
    addFlash (OperationFailed, fromJust mMismatchMessage)

  authorsiglink <- guardJust $ getAuthorSigLink document
  let ispreparation = documentstatus document == Preparation
      isauthor = (Just $ userid user) == maybesignatory authorsiglink
      isincompany = isJust (maybecompany authorsiglink) &&
                      usercompany user == maybecompany authorsiglink
      isauthororincompany = isauthor || isincompany
      isattachment = isAttachment document
      msiglink = find (isSigLinkFor $ userid user) $ documentsignatorylinks document

  ctx <- getContext
  case (ispreparation, msiglink) of
    (True,  _) | isattachment        -> pageAttachmentDesign document
    (True,  _)                       -> pageDocumentDesign document
    (False, _) | isauthororincompany -> pageDocumentView document msiglink
    (False, Just siglink)            -> pageDocumentSignView ctx document siglink
    _                                -> internalError

{- |
   Modify a document. Typically called with the "Underteckna" or "Save" button
   If document is in preparation, we move it to pending
   If document is in AwaitingAuthor, we move it to Closed
   Otherwise, we do mzero (NOTE: this is not the correct action to take)
   User must be logged in.
   Document must exist
   User must be author
   URL: /d/{documentid}
   Method: POST
 -}
handleIssueShowPost :: Kontrakcja m => DocumentID -> m KontraLink
handleIssueShowPost docid = withUserPost $ do
  document <- guardRightM $ getDocByDocID docid
  Context { ctxmaybeuser = muser } <- getContext
  unless (isAuthor (document, muser)) internalError -- still need this because others can read document
  sign              <- isFieldSet "sign"
  send              <- isFieldSet "send"
  -- Behold!
  case documentstatus document of
    Preparation | sign              -> handleIssueSign                 document
    Preparation | send              -> handleIssueSend                 document
    AwaitingAuthor                  -> handleIssueSignByAuthor         document
    _ -> return $ LinkContracts

handleIssueSign :: Kontrakcja m => Document -> m KontraLink
handleIssueSign document = do
    Log.debug "handleIssueSign"
    ctx <- getContext
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual document) docs
        case (lefts mndocs, rights mndocs) of
          ([], [d]) -> do
            addFlashM $ modalSendConfirmationView d
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
            if isJust $ ctxservice ctx
              then do
              --sessionid <- readCookieValue "sessionId"
              --return $ LinkConnectUserToSession (ctxservice ctx) (fromJust $ ctxmaybeuser ctx) sessionid LinkCSVLandPage
              return $ LinkCSVLandPage (length ds)
              else do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              case documenttype (head ds) of
                Signable Contract -> return $ LinkContracts
                Signable Offer    -> return $ LinkOffers
                Signable Order    -> return $ LinkOrders
                _                 -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSign had lefts: " ++ intercalate ";" (map show ls)
            return LoopBack
      Left link -> return link
    where
      forIndividual :: Kontrakcja m => Document -> Document -> m (Either KontraLink Document)
      forIndividual udoc doc = do
        mprovider <- readField "eleg"
        mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocument (documentid doc) Nothing
                   Just provider -> do
                      signature     <- getDataFnM $ look "signature"
                      transactionid <- getDataFnM $ look "transactionid"
                      esinfo <- BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                      case esinfo of
                        BankID.Problem msg -> return $ Left msg
                        BankID.Mismatch msg _ _ _ -> return $ Left msg
                        BankID.Sign sinfo -> Right <$>  authorSignDocument (documentid doc) (Just sinfo)
        case mndoc of
          Right (Right newdocument) -> do
            postDocumentChangeAction newdocument udoc Nothing
            return $ Right newdocument
          _ -> return $ Left LoopBack


handleIssueSend :: Kontrakcja m => Document -> m KontraLink
handleIssueSend document = do
    Log.debug "handleIssueSend"
    ctx <- getContext
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM (forIndividual document) docs
        case (lefts mndocs, rights mndocs) of
          ([], [d]) -> do
            addFlashM $ modalSendConfirmationView d
            return $ LinkIssueDoc (documentid d)
          ([], ds) -> do
            if isJust $ ctxservice ctx
              then do
              --sessionid <- readCookieValue "sessionId"
              --return $ LinkConnectUserToSession (ctxservice ctx) (fromJust $ ctxmaybeuser ctx) sessionid LinkCSVLandPage
              return $ LinkCSVLandPage (length ds)
              else do
              addFlashM $ flashMessageCSVSent $ length ds
              Log.debug (show $ map documenttype ds)
              case documenttype (head ds) of
                Signable Contract -> return $ LinkContracts
                Signable Offer    -> return $ LinkOffers
                Signable Order    -> return $ LinkOrders
                _ -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSend had lefts: " ++ intercalate ";" (map show ls)
            internalError
      Left link -> return link
    where
      forIndividual udoc doc = do
        mndoc <- authorSendDocument (documentid doc)
        case mndoc of
          Right newdocument -> do
            postDocumentChangeAction newdocument udoc Nothing
            return ()
          Left _ -> return ()
        return mndoc

{- |
    If the document has a multiple part this will pump csv values through it to create multiple docs, and then
    save the original as a template if it isn't already.  This will make sure to clean the csv data.  It just returns
    a list containing the original doc on it's own, if the doc hasn't got a multiple part.

    I feel like this is quite dangerous to do all at once, maybe need a transaction?!
-}
splitUpDocument :: Kontrakcja m => Document -> m (Either KontraLink [Document])
splitUpDocument doc = do
  case (msum (signatorylinkcsvupload <$> documentsignatorylinks doc), getCSVCustomFields doc) of
    (Just _, Left msg) -> do
      Log.debug $ "splitUpDocument: got csvupload, but getCSVCustomFields returned issues: " ++ show msg
      internalError
    (Nothing, _) -> do
      Log.debug $ "splitUpDocument called on document without csvupload, that is ok"
      return $ Right [doc]
    (Just csvupload, Right csvcustomfields) -> do
      Log.debug $ "splitUpDocument called on document with csvupload and we managed to split fields properly"
      case (cleanCSVContents (ELegitimationIdentification `elem` (documentallowedidtypes doc)) (length csvcustomfields) $ csvcontents csvupload) of
        (_prob:_, _) -> do
          addFlashM flashMessageInvalidCSV
          Log.debug $ "splitUpDocument: back to document"
          return $ Left $ LinkDesignDoc $ (documentid doc)
        ([], CleanCSVData{csvbody}) -> do
          mdocs <- mapM (createDocFromRow doc) csvbody
          if Data.List.null (lefts mdocs)
            then do
              Log.debug $ "splitUpDocument: finishing properly"
              return $ Right (rights mdocs)
            else do
              Log.debug $ "splitUpDocument: createDocFromRow returned some Lefts: " ++ show (lefts mdocs)
              internalError
  where createDocFromRow udoc xs = do
          actor <- guardJustM $ mkAuthorActor <$> getContext
          runDBUpdate $ DocumentFromSignatoryData (documentid udoc) (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs) actor
          where item n | n<(length xs) = xs !! n
                       | otherwise = ""

handleIssueSignByAuthor :: Kontrakcja m => Document -> m KontraLink
handleIssueSignByAuthor doc = do
     mprovider <- readField "eleg"
     mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocumentFinal (documentid doc) Nothing
                   Just provider -> do
                      signature     <- getDataFnM $ look "signature"
                      transactionid <- getDataFnM $ look "transactionid"
                      esinfo <- BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                      case esinfo of
                        BankID.Problem msg -> return $ Left msg
                        BankID.Mismatch msg _ _ _ -> return $ Left msg
                        BankID.Sign sinfo -> Right <$>  authorSignDocumentFinal (documentid doc) (Just sinfo)

     case mndoc of
         Right (Right ndoc) -> do
             postDocumentChangeAction ndoc doc Nothing
             addFlashM flashAuthorSigned
             return $ LinkIssueDoc (documentid doc)
         _ -> return LoopBack

-- | Check if current user is author or has company rights to view the document
withAuthorisedViewer :: Kontrakcja m => DocumentID -> m (Either KontraLink a) -> m (Either KontraLink a)
withAuthorisedViewer docid action = do
  _ <- guardRightM $ getDocByDocID docid
  action

{- |
   Show the document with title in the url
   URL: /d/{documentid}/{title}
   Method: GET
 -}
handleFileGet :: Kontrakcja m => FileID -> String -> m (Either KontraLink Response)
handleFileGet fileid' _title = do
  withUserGet $ onlyAdmin $ do
   ctx <- getContext
   contents <- liftIO $ getFileIDContents ctx fileid'

   if BS.null contents
      then internalError
      else do
          let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          let res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
          return res2

{- |
   Save a document from data in the post params.

 -}

handleAttachmentViewForViewer :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handleAttachmentViewForViewer docid siglinkid mh = do
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  disableLocalSwitch
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> maybeScheduleRendering file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

handleAttachmentViewForAuthor :: Kontrakcja m => DocumentID -> m Response
handleAttachmentViewForAuthor docid = do
  doc <- guardRightM $ getDocByDocID docid
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM (\file -> maybeScheduleRendering file (documentid doc)) f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => DocumentID -> FileID -> m JSValue
handleFilePages did fid = do
  (mdoc,_) <- jsonDocumentGetterWithPermissionCheck did
  when (isNothing mdoc) internalError
  let doc = fromJust mdoc
  let allfiles = (documentfiles doc) ++ (documentsealedfiles doc)  ++ (authorattachmentfile <$> documentauthorattachments doc)
  case find (== fid) allfiles of
    Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString $ "File #" ++ show fid ++ " not found in document #" ++ show did)]
    Just _  -> do
      jpages <- maybeScheduleRendering fid did
      case jpages of
       JpegPagesPending -> return $ JSObject $ toJSObject [("wait",JSString $ toJSString "Temporary unavailable (file is still pending)")]
       JpegPagesError _ -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "rendering failed")]
       JpegPages pages  -> return $ JSObject $ toJSObject [("pages",JSArray $ map pageinfo pages)]
  where
      pageinfo (_,width,height) = JSObject $ toJSObject [("width",JSRational True $ toRational width),
                                                         ("height",JSRational True $ toRational height)
                                                        ]

handlePageOfDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink Response)
handlePageOfDocument docid = checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    handlePageOfDocument' docid $ Just (siglinkid, sigmagichash)

handlePageOfDocument' :: Kontrakcja m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> m Response
handlePageOfDocument' documentid mtokens = do
  Log.debug $ "Request for doc " ++ show documentid
  edoc <- case mtokens of
    Nothing         -> getDocByDocID documentid
    Just (slid, mh) -> getDocByDocIDSigLinkIDAndMagicHash documentid slid mh
  case edoc of
    Left l -> do
      Log.debug ("Could not get Document " ++ show l)
      internalError
    Right Document { documentfiles
                   , documentsealedfiles
                   , documentstatus
                   } -> do
      let pending JpegPagesPending = True
          pending _                = False
          files                    = if documentstatus == Closed
                                      then documentsealedfiles
                                      else documentfiles
      case files of
         [] -> notFound $ toResponse "temporarily unavailable (document has no files)"
         f  -> do
             b <- mapM (\file -> maybeScheduleRendering file documentid) f
             if any pending b
                then notFound (toResponse "temporarily unavailable (document has files pending for process)")
                else do
                    pages <- Doc.DocView.showFilesImages2 documentid mtokens $ zip f b
                    simpleResponse pages

handleDocumentUpload :: Kontrakcja m => DocumentID -> BS.ByteString -> String -> m ()
handleDocumentUpload docid content1 filename = do
  Log.debug $ "Uploading file for doc #" ++ show docid
  fileresult <- attachFile docid filename content1
  case fileresult of
    Left err -> do
      Log.debug $ "Got an error in handleDocumentUpload: " ++ show err
      return ()
    Right _document ->
        return ()
  return ()

handleIssueNewDocument :: Kontrakcja m => m KontraLink
handleIssueNewDocument = withUserPost $ do
    Log.debug $ "Creating a new document"
    input <- getDataFnM (lookInput "doc")
    mdocprocess <- getDocProcess
    let docprocess = fromMaybe (Contract) mdocprocess
    Log.debug $ "Creating new document of process : " ++ show docprocess
    mdoc <- makeDocumentFromFile (Signable docprocess) input 1
    case mdoc of
      Nothing -> return LinkUpload
      Just doc -> do
        Log.debug $ "Document #" ++ show (documentid doc) ++ " created"
        _ <- addDocumentCreateStatEvents doc
        return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontrakcja m => m KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile (Template Contract) input 1
  case mdoc of
    Nothing -> return $ LinkTemplates
    Just doc -> do
      _ <- addDocumentCreateStatEvents doc
      return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontrakcja m => m KontraLink
handleCreateNewAttachment = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile Attachment input 0
  when (isJust mdoc) $ do
    _<- addDocumentCreateStatEvents $ fromJust mdoc
    return ()
  return LinkAttachments

makeDocumentFromFile :: Kontrakcja m => DocumentType -> Input -> Int -> m (Maybe Document)
makeDocumentFromFile doctype (Input contentspec (Just filename) _contentType) nrOfExtraSigs  = do
    Log.debug $ "makeDocumentFromFile: beggining"
    guardLoggedIn
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    if BSL.null content
      then do
        Log.debug "makeDocumentFromFile: no content"
        return Nothing
      else do
          Log.debug "Got the content, creating document"
          let title = basename filename
          doc <- guardRightM $ newDocument title doctype nrOfExtraSigs
          handleDocumentUpload (documentid doc) (concatChunks content) title
          return $ Just doc
makeDocumentFromFile _ _ _ = internalError -- to complete the patterns


handleRubbishRestore :: Kontrakcja m => m KontraLink
handleRubbishRestore = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> guardRightM $ runDBUpdate $ RestoreArchivedDocument user did actor) docids
  addFlashM flashMessageRubbishRestoreDone
  return $ LinkRubbishBin

handleRubbishReallyDelete :: Kontrakcja m => m KontraLink
handleRubbishReallyDelete = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  ctx <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> do
            doc <- guardRightM $ runDBUpdate $ ReallyDeleteDocument user did actor
            case getSigLinkFor doc user of
              Just sl -> runDB $ addSignStatPurgeEvent doc sl (ctxtime ctx)
              _ -> return False)
    docids
  addFlashM flashMessageRubbishHardDeleteDone
  return $ LinkRubbishBin

handleTemplateShare :: Kontrakcja m => m KontraLink
handleTemplateShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleTemplateShareDone $ documenttitle d
      _ -> addFlashM flashMessageMultipleTemplateShareDone
    return $ LinkTemplates

handleAttachmentShare :: Kontrakcja m => m KontraLink
handleAttachmentShare = withUserPost $ do
    docs <- handleIssueShare
    case docs of
      (d:[]) -> addFlashM $ flashMessageSingleAttachmentShareDone $ documenttitle d
      _ -> addFlashM  flashMessageMultipleAttachmentShareDone
    return $ LinkAttachments

handleIssueShare :: Kontrakcja m => m [Document]
handleIssueShare = do
  ids <- getCriticalFieldList asValidDocID "doccheck"
  _ <- runDBUpdate $ SetDocumentSharing ids True
  w <- flip mapM ids $ (runDBQuery . GetDocumentByDocumentID)
  return (catMaybes w)

handleAttachmentRename :: Kontrakcja m => DocumentID -> m KontraLink
handleAttachmentRename docid = withUserPost $ do
  newname <- getCriticalField return "docname"
  actor <- guardJustM $ mkAuthorActor <$> getContext
  doc <- guardRightM $ runDBUpdate $ SetDocumentTitle docid newname actor
  return $ LinkIssueDoc $ documentid doc

handleBulkContractRemind :: Kontrakcja m => m KontraLink
handleBulkContractRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Contract)
    return $ LinkContracts

handleBulkOfferRemind :: Kontrakcja m => m KontraLink
handleBulkOfferRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Offer)
    return $ LinkOffers

handleBulkOrderRemind :: Kontrakcja m => m KontraLink
handleBulkOrderRemind = withUserPost $ do
    _ <- handleIssueBulkRemind (Signable Order)
    return $ LinkOrders

{- |
    This sends out bulk reminders.  The functionality is offered in the document
    and offers list page.  It will make sure the user is actually the author of everything,
    and send out reminders only to signatories who haven't accepted or signed on those that are
    pending.  This returns all the signatory links that were reminded.
-}
handleIssueBulkRemind :: Kontrakcja m => DocumentType -> m [SignatoryLink]
handleIssueBulkRemind doctype = do
    ctx@Context{ctxmaybeuser = Just user } <- getContext
    ids <- getCriticalFieldList asValidDocID "doccheck"
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
    case (length remindedsiglinks) of
      0 -> addFlashM $ flashMessageNoBulkRemindsSent doctype
      _ -> addFlashM $ flashMessageBulkRemindsSent doctype
    return remindedsiglinks
    where
      docRemind :: Kontrakcja m => Context -> User -> DocumentID -> m [SignatoryLink]
      docRemind ctx user docid = do
        doc <- queryOrFail $ GetDocumentByDocumentID docid
        case (documentstatus doc) of
          Pending -> do
            let isElegible = isEligibleForReminder (Just user) doc
                unsignedsiglinks = filter isElegible $ documentsignatorylinks doc
            sequence . map (sendReminderEmail Nothing ctx doc) $ unsignedsiglinks
          _ -> return []

{- |
   Get some html to display the images of the files
   URL: /pagesofdoc/{documentid}
   Method: GET
   FIXME: Should probably check for permissions to view
 -}
showPage :: Kontrakcja m => DocumentID -> FileID -> Int -> m (Either KontraLink Response)
showPage docid fileid pageno = do
  msid <- readField "signatorylinkid"
  mmh <- readField "magichash"
  edoc <- case (msid, mmh) of
           (Just sid, Just mh) -> Right <$> guardRightM (getDocByDocIDSigLinkIDAndMagicHash docid sid mh)
           _ ->  checkUserTOSGet $ guardRightM (getDocByDocID docid)
  case edoc of
       Right doc -> do
           unless (fileInDocument doc fileid) internalError
           Right <$> showPage' fileid pageno
       Left rdir -> return $ Left rdir


showPreview:: Kontrakcja m => DocumentID -> FileID -> m (Either KontraLink Response)
showPreview docid fileid = withAuthorisedViewer docid $ do
    iprev <- preview docid fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid Nothing fileid

showPreviewForSignatory:: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m (Either KontraLink Response)
showPreviewForSignatory docid siglinkid sigmagichash fileid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    iprev <- preview docid fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid (getMaybeSignatoryLink (doc,siglinkid)) fileid

preview :: Kontrakcja m =>  DocumentID -> FileID -> Int -> m (Maybe Response)
preview did fid value
  | value > 10 = return Nothing
  | otherwise  =   do
        Context{ctxnormalizeddocuments} <- getContext
        docmap <- liftIO $ readMVar ctxnormalizeddocuments
        case Map.lookup fid docmap of
            Just (JpegPages pages) -> do
                let (contents,_,_) =  pages !! 0
                scaledContent <- liftIO $ scaleForPreview did contents
                let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [scaledContent]) Nothing
                return $ Just $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
            other -> do
                when_ (other == Nothing) $ maybeScheduleRendering fid did
                liftIO $ threadDelay 500000
                preview did fid (value+1)


showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- getContext
  docmap <- liftIO $ readMVar ctxnormalizeddocuments
  case Map.lookup fileid docmap of
    Just (JpegPages pages) -> do
      let (contents,_,_) =  pages !! (pageno - 1)
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      Log.debug $ "JPEG page found and returned for file " ++ show fileid ++ " and page " ++ show pageno
      return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
    _ -> do
      Log.debug $ "JPEG page not found in cache, responding 404 for file " ++ show fileid ++ " and page " ++ show pageno
      notFound (toResponse "temporarily unavailable (document has files pending for process)")

handleCancel :: Kontrakcja m => DocumentID -> m KontraLink
handleCancel docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  actor <- guardJustM $ mkAuthorActor <$> getContext
  if isPending doc || isAwaitingAuthor doc
    then do
    mdoc' <- runDBUpdate $ CancelDocument (documentid doc) ManualCancel actor
    case mdoc' of
      Right doc' ->  do
          Log.debug $ "Canceling document #" ++ show docid
          addFlashM $ flashMessageCanceled doc'
      Left errmsg -> addFlash (OperationFailed, errmsg)
    else addFlashM flashMessageCannotCancel
  return (LinkIssueDoc $ documentid doc)

{-
handleWithdrawn:: DocumentID -> Kontra KontraLink
handleWithdrawn docid = do
  mdoc <- runDBQuery $ GetDocumentByDocumentID docid
  case (mdoc) of
    Just doc -> withDocumentAutho doc $ do
                          runDBUpdate $ WithdrawnDocument $ documentid doc
                          return (LinkIssueDoc $ documentid doc)
    Nothing -> return LinkMain
-}

handleRestart :: Kontrakcja m => DocumentID -> m KontraLink
handleRestart docid = withUserPost $ do
  doc <- guardRightM $ getDocByDocID docid
  doc2 <- guardRightM $ restartDocument doc
  addFlashM $ flashDocumentRestarted doc2
  return $ LinkIssueDoc (documentid doc2)

handleResend :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleResend docid signlinkid  = withUserPost $ do
  ctx@Context { ctxmaybeuser = Just user } <- getContext
  doc <- guardRightM $ getDocByDocID docid
  unless (isAuthor (doc, user)) internalError -- only author can resend
  signlink <- guardJust $ getSigLinkFor doc signlinkid
  customMessage <- getCustomTextField "customtext"
  _ <- sendReminderEmail customMessage ctx doc signlink
  addFlashM $ flashRemindMailSent signlink
  return (LinkIssueDoc docid)

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> do
      edoc <- getDocByDocID docid
      case edoc of
        Left _ -> return LoopBack
        Right doc -> do
          unless (isAuthor (doc, user)) internalError
          muser <- runDBQuery $ GetUserByEmail (documentservice doc) (Email email)
          actor <- guardJustM $ mkAuthorActor <$> getContext
          mnewdoc <- runDBUpdate $ ChangeSignatoryEmailWhenUndelivered docid slid muser email actor
          case mnewdoc of
            Right newdoc -> do
              -- get (updated) siglink from updated document
              sl <- guardJust (getSigLinkFor newdoc slid)
              ctx <- getContext
              _ <- sendInvitationEmail1 ctx newdoc sl
              return $ LoopBack
            _ -> return LoopBack
    _ -> return LoopBack

checkLinkIDAndMagicHash :: Kontrakcja m => Document -> SignatoryLinkID -> MagicHash -> m ()
checkLinkIDAndMagicHash document linkid magichash1 = do
  siglink <- guardJust $ getSigLinkFor document linkid
  unless (signatorymagichash siglink == magichash1) internalError
  return ()

handleShowUploadPage :: Kontrakcja m => m (Either KontraLink String)
handleShowUploadPage = checkUserTOSGet $ do
    showTemplates <- isFieldSet "showTemplates"
    tooLarge <- isFieldSet "tooLarge"
    mdocprocess <- getDocProcess
    when tooLarge $ addFlashM modalPdfTooLarge
    uploadPage mdocprocess showTemplates

getDocProcess :: Kontrakcja m => m (Maybe DocumentProcess)
getDocProcess = getOptionalField asDocType "doctype"
  where
    asDocType :: String -> Result DocumentProcess
    asDocType val
      | val == show Offer = Good $ Offer
      | val == show Contract = Good $ Contract
      | val == show Order = Good $ Order
      | otherwise = Empty

handleCreateFromTemplate :: Kontrakcja m => m KontraLink
handleCreateFromTemplate = withUserPost $ do
  docid <- readField "template"
  Log.debug $ show "Creating document from template : " ++ show docid
  case docid of
    Just did -> do
      user <- guardJustM $ ctxmaybeuser <$> getContext
      document <- queryOrFail $ GetDocumentByDocumentID $ did
      Log.debug $ show "Matching document found"
      auid <- guardJust $ join $ maybesignatory <$> getAuthorSigLink document
      auser <- guardJustM $ runDBQuery $ GetUserByID auid
      let haspermission = (userid auser == userid user) ||
                          ((usercompany auser == usercompany user && (isJust $ usercompany user)) &&  isDocumentShared document)
      enewdoc <- if (isTemplate document && haspermission)
                    then do
                      Log.debug $ show "Valid persmision to create from template"
                      mcompany <- getCompanyForUser user
                      actor <- guardJustM $ mkAuthorActor <$> getContext
                      runDBUpdate $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor
                    else internalError
      case enewdoc of
        Right newdoc -> do
          _ <- addDocumentCreateStatEvents newdoc
          Log.debug $ show "Document created from template"
          return $ LinkIssueDoc $ documentid newdoc
        Left _ -> internalError
    Nothing -> internalError

{- |
   Download the attachment with the given fileid
 -}

handleDownloadFile :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
handleDownloadFile did fid _nameForBrowser = do
  msid <- readField "signatorylinkid"
  mmh <- readField "magichash"
  doc <- case (msid, mmh) of
           (Just sid, Just mh) -> guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
           _ ->                   guardRightM $ getDocByDocID did
  unless (fileInDocument doc fid) internalError
  respondWithFile doc fid

respondWithFile :: Kontrakcja m =>  Document -> FileID -> m Response
respondWithFile _doc fid =  do
    ctx <- getContext
    respondWithPDF =<< liftIO (getFileIDContents ctx fid)

respondWithPDF :: Kontrakcja m => BS.ByteString -> m Response
respondWithPDF contents = do
  let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
      res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
  return res2

handleDeleteSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  m KontraLink
handleDeleteSigAttach docid siglinkid = do
  mh <- guardJustM $ readField "magichash"
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  fid <- read <$> getCriticalField asValidID "deletesigattachment"
  Context{ctxtime, ctxipnumber} <- getContext
  let email = getEmail siglink
  Log.debug $ "delete Sig attachment " ++ (show fid) ++ "  " ++ email
  _ <- runDBUpdate $ DeleteSigAttachment docid siglinkid fid
       (SignatoryActor ctxtime ctxipnumber (maybesignatory siglink) email siglinkid)
  return $ LinkSignDoc doc siglink

handleSigAttach :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleSigAttach docid siglinkid = do
  mh <- guardJustM $ readField "magichash"
  doc <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash docid siglinkid mh
  siglink <- guardJust $ getSigLinkFor doc siglinkid
  attachname <- getCriticalField asValidFieldValue "attachname"
  let email = getEmail siglink
  _ <- guardJust $  find (\sa -> signatoryattachmentname sa == attachname) (signatoryattachments siglink)
  (Input contentspec _ _) <- getDataFnM (lookInput "sigattach")
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
  file <- runDB $ dbUpdate $ NewFile attachname content
  let actor = SignatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory siglink) email siglinkid
  d <- guardRightM $ runDBUpdate $ SaveSigAttachment docid siglinkid attachname (fileid file) actor
  return $ LinkSignDoc d siglink

jsonDocument :: Kontrakcja m => DocumentID -> m JSValue
jsonDocument did = do
    (mdoc,msiglink) <- jsonDocumentGetterWithPermissionCheck did
    cttime <- liftIO $ getMinutesTime
    rsp <- case mdoc of
         Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "No document avaible")]
         Just doc -> do
             switchLocale (getLocale doc)
             JSObject <$> documentJSON msiglink cttime doc
    return rsp

jsonDocumentGetterWithPermissionCheck ::   Kontrakcja m => DocumentID -> m (Maybe Document, Maybe SignatoryLink)
jsonDocumentGetterWithPermissionCheck did = do
    ctx <- getContext
    mmagichashh <- readField "magichash"
    msignatorylink <- readField "signatoryid"
    case (msignatorylink,mmagichashh) of
        (Just slid,Just mh) -> do
                   mdoc <- runDBQuery $ GetDocumentByDocumentID did
                   let msiglink = join $ find ((== slid) . signatorylinkid) <$> (documentsignatorylinks <$> mdoc)
                   if (validSigLink slid mh mdoc)
                     then return (mdoc,msiglink)
                     else return (Nothing,Nothing)
        _ -> do
                    mdoc <- toMaybe <$> getDocByDocID did
                    let msiglink = join $ getMaybeSignatoryLink <$> pairMaybe mdoc (ctxmaybeuser ctx)
                    return $ (mdoc, msiglink)



handleInvariantViolations :: Kontrakcja m => m Response
handleInvariantViolations = onlyAdmin $ do
  Context{ ctxtime } <- getContext
  docs <- runDBQuery $ GetDocumentsByService Nothing
  let probs = listInvariantProblems ctxtime docs
      res = case probs of
        [] -> "No problems!"
        _  -> intercalate "\n" probs
  return $ Response 200 Map.empty nullRsFlags (BSL.fromString res) Nothing



prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    mailtype <- getField' "mailtype"
    doc <- guardJustM $ runDBQuery $ GetDocumentByDocumentID docid
    ctx <- getContext
    content <- case mailtype of
         "remind" -> do
             let msl = find ((== slid) . signatorylinkid) $ documentsignatorylinks doc
             case msl of
               Just sl -> mailDocumentRemindContent  Nothing ctx doc sl
               Nothing -> return ""
         "reject" -> do
             let msl = find ((== slid) . signatorylinkid) $ documentsignatorylinks doc
             case msl of
               Just sl -> mailDocumentRejectedContent Nothing ctx  doc sl
               Nothing -> return ""
         "invite" -> mailInvitationContent False ctx Sign doc Nothing
         _ -> return ""
    return $ JSObject $ toJSObject [("content",JSString $ toJSString $ content)]


handleCSVLandpage :: Kontrakcja m => Int -> m String
handleCSVLandpage c = do
  text <- csvLandPage c
  return text

-- Function for saving document while still working in design view
handleSaveDraft:: Kontrakcja m => DocumentID -> m JSValue
handleSaveDraft did = do
    doc       <- guardRightM $ getDocByDocID did
    draftData <- guardJustM $ withJSONFromField "draft" $ fromJSON
    actor     <- guardJustM $ mkAuthorActor <$> getContext
    res       <- applyDraftDataToDocument doc draftData actor
    case res of
         Right _ -> return $ JSObject $ toJSObject []
         Left s -> do
             return $ JSObject $ toJSObject [("error",JSString $ toJSString $ "Document saving failed with ("++s++") - unless someone is experimenting this should never happend")]


handleSetAttachments :: Kontrakcja m => DocumentID -> m KontraLink
handleSetAttachments did = do
    doc <- guardRightM $ getDocByDocID did
    attachments <- getAttachments 0
    Log.debug $ "Setting attachments to " ++ show attachments
    actor <- guardJustM $ mkAuthorActor <$> getContext
    forM_ (documentauthorattachments doc) $ \att -> runDB $ dbUpdate $ RemoveDocumentAttachment did (authorattachmentfile att) actor
    forM_ (nub attachments) $ \att -> runDB $ dbUpdate $ AddDocumentAttachment did att actor -- usage of nub is ok, as we never expect this list to be big
    return LoopBack
   where
        getAttachments :: Kontrakcja m => Int -> m [FileID]
        getAttachments i = do
            mf <- tryGetFile i
            case mf of
                 Just f -> (f:) <$> getAttachments (i+1)
                 Nothing -> return []
        tryGetFile ::  Kontrakcja m => Int -> m (Maybe FileID)
        tryGetFile i = do
            inp <- getDataFn' (lookInput $ "attachment_" ++ show i)
            case inp of
                 Just (Input (Left filepath) (Just filename) _contentType) -> do
                     content <- liftIO $ BSL.readFile filepath
                     let title = basename filename
                     doc <- guardRightM $ newDocument title Attachment 0
                     doc' <- guardRightM $  attachFile (documentid doc) title (concatChunks content)
                     return $ listToMaybe $ documentfiles  doc'
                 Just (Input  (Right c)  _ _)  -> do
                      case maybeRead (BSL.toString c) of
                          Just fid -> (fmap fileid) <$> (runDB $ dbQuery $ GetFileByFileID fid)
                          Nothing -> return $ Nothing
                 _ -> return Nothing

handleUpsalesDeleted :: Kontrakcja m => m Response
handleUpsalesDeleted = onlyAdmin $ do
  docs <- runDBQuery $ GetDocumentsByService $ Just $ ServiceID $ BS.fromString "upsales"
  let deleteddocs = [[show $ documentid d, showDateYMD $ documentctime d, documenttitle d]
                    | d <- docs
                    , isDeletedFor $ getAuthorSigLink d
                    , (isJust $ getSigLinkFor d SignatoryAuthor) && (isJust $ getSigLinkFor d $ unsafeCompanyID 1849610088)]
  let header = ["document_id", "date created", "document_title"]
  let csv = toCSV header deleteddocs
  ok $ setHeader "Content-Disposition" "attachment;filename=upsalesdocsdeleted.csv"
     $ setHeader "Content-Type" "text/csv"
     $ toResponse csv

handleParseCSV :: Kontrakcja m => m JSValue
handleParseCSV = do
  ctx <- getContext
  guardJust $ ctxmaybeuser ctx
  Log.debug "Csv parsing"
  customfieldscount <- guardJustM $ maybeReadIntM $ getField "customfieldscount"
  input <- getDataFn' (lookInput "csv")
  eleg <- isFieldSet "eleg"
  res <- case input of
        Just(Input contentspec (Just filename) _ ) -> do
          content <- case contentspec of
                       Left filepath -> liftIO $ BSL.readFile filepath
                       Right content -> return content
          let _title = BS.fromString (basename filename)
          case parseCSV content of
                 Left _ -> oneProblemJSON $ renderTemplateM "flashMessageFailedToParseCSV" ()
                 Right contents
                   | length contents > 1000 -> oneProblemJSON $ renderTemplateFM "flashMessageCSVHasTooManyRows" $ field "maxrows" (1000::Int)
                   | otherwise -> do
                       let (problems, csvdata) = cleanCSVContents eleg customfieldscount contents
                       liftIO $ json $ do
                            JSON.field "problems" $ for problems $ \p -> do
                                JSON.field "description" $ ((runTemplatesT (ctxlocale ctx, ctxglobaltemplates ctx) $ csvProblemToDescription p) :: IO String)
                                when (isJust $ problemRow p) $
                                    JSON.field "row" $ fromJust $ problemRow p
                                when (isJust $ problemCell p) $
                                    JSON.field "cell" $ fromJust $ problemCell p
                            JSON.field "rows" $ csvbody csvdata

        _ -> do
            oneProblemJSON $ renderTemplateM "flashMessageFailedToParseCSV" ()
  return res
  where
      oneProblemJSON :: Kontrakcja m => m String -> m JSValue
      oneProblemJSON ms = do
          s <- ms
          liftIO $ json $ do
            JSON.field "problems" $ do
                JSON.field "description" s
            JSON.field "rows" ([]::[String])
