{- |
   DocControl represents the controler (in MVC) of the document.
 -}
module Doc.DocControl(
    -- Exported utils or test functions
      sendReminderEmail
    -- Top level handlers
    , handleDownloadFile
    , handleDownloadMainFile
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
    , handleAttachmentRename
    , handleCreateNewAttachment
    , handleCreateNewTemplate
    , handleIssueShowGet
    , handleIssueNewDocument
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
    , handleShowVerificationPage
    , handleVerify
) where

import AppView
import DB
import DBError
import Doc.Action
import Doc.CSVUtils
import Doc.Model
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocStateUpdate
import Doc.Rendering
import Doc.DocUtils
import Doc.DocView
import Doc.DocViewMail
import qualified Doc.DocSeal as DocSeal
import InputValidation
import File.Model
import File.Storage
import Kontra
import KontraLink
import MagicHash
import MinutesTime
import Misc
import Redirect
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Templates.Templates
import Util.CSVUtil
import Util.FlashUtil
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Util.MonadUtils
import Doc.Invariants
import Stats.Control
import User.Utils
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
import Text.JSON.Gen hiding (value)
import Text.JSON.String (runGetJSON)
import qualified Text.JSON.Gen as J
import Text.JSON.FromJSValue
import Doc.DocDraft as Draft
import qualified User.Action
import qualified ELegitimation.BankID as BankID
import Util.Actor
import PadQueue.Model
import qualified Templates.Fields as F
import qualified MemCache as MemCache
import qualified GuardTime as GuardTime
import System.IO.Temp
import System.Directory
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
  when (document `allowsIdentification` PadIdentification) internalError
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
  fieldsJSON <- guardRightM $ liftM (runGetJSON readJSArray) $ getField' "fields"
  fields <- guardJustM $ withJSValue fieldsJSON $ fromJSValueCustomMany $ do
      ft <- fromJSValueM
      value <- fromJSValueField "value"
      return $ pairMaybe ft value
  mprovider <- readField "eleg"
  edoc <- case mprovider of
           Nothing -> Right <$> signDocumentWithEmailOrPad documentid signatorylinkid magichash fields
           Just provider -> do
             transactionid <- getDataFnM $ look "transactionid"
             esigninfo <- case provider of
               MobileBankIDProvider -> do
                 BankID.verifySignatureAndGetSignInfoMobile documentid signatorylinkid magichash transactionid
               _ -> do
                 signature <- getDataFnM $ look "signature"
                 BankID.verifySignatureAndGetSignInfo documentid signatorylinkid magichash provider signature transactionid
             case esigninfo of
               BankID.Problem msg -> return $ Left msg
               BankID.Mismatch msg sfn sln spn -> do
                 document <- guardRightM $ getDocByDocIDSigLinkIDAndMagicHash documentid signatorylinkid magichash
                 handleMismatch document signatorylinkid msg sfn sln spn
                 return $ Left msg
               BankID.Sign sinfo -> Right <$> signDocumentWithEleg documentid signatorylinkid magichash fields sinfo
  case edoc of
    Right (Right (doc, olddoc)) -> do
      postDocumentPendingChange doc olddoc "web"
      udoc <- guardJustM $ dbQuery $ GetDocumentByDocumentID documentid
      handleAfterSigning udoc signatorylinkid
      return LoopBack
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

handleMismatch :: Kontrakcja m => Document -> SignatoryLinkID -> String -> String -> String -> String -> m ()
handleMismatch doc sid msg sfn sln spn = do
        ctx <- getContext
        let Just sl = getSigLinkFor doc sid
        Log.eleg $ "Information from eleg did not match information stored for signatory in document." ++ show msg
        Right newdoc <- dbUpdate $ CancelDocument (documentid doc) (ELegDataMismatch msg sid sfn sln spn)
                        (signatoryActor (ctxtime ctx)
                         (ctxipnumber ctx)
                         (maybesignatory sl)
                         (getEmail sl)
                         sid)
        postDocumentCanceledChange newdoc "web+eleg"

{- |
    Call after signing in order to save the document for any user, and
    put up the appropriate modal.
-}
handleAfterSigning :: Kontrakcja m => Document -> SignatoryLinkID -> m ()
handleAfterSigning document@Document{documentid} signatorylinkid = do
  ctx <- getContext
  signatorylink <- guardJust $ getSigLinkFor document signatorylinkid
  maybeuser <- dbQuery $ GetUserByEmail (currentServiceID ctx) (Email $ getEmail signatorylink)
  case maybeuser of
    Just user | isJust $ userhasacceptedtermsofservice user-> do
      let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx)  (maybesignatory signatorylink) (getEmail signatorylink) (signatorylinkid)
      _ <- dbUpdate $ SaveDocumentForUser documentid user signatorylinkid actor
      return ()
    _ -> return ()


rejectDocumentIphoneCase :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m KontraLink
rejectDocumentIphoneCase did sid _ = rejectDocument did sid

{- |
   Control rejecting the document
   URL: /s/{docid}/{signatorylinkid1}/{magichash1}
 -}
rejectDocument :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
rejectDocument documentid siglinkid = do
  magichash <- guardJustM $ readField "magichash"
  customtext <- getCustomTextField "customtext"

  edocs <- rejectDocumentWithChecks documentid siglinkid magichash customtext

  case edocs of
    Left (DBActionNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left (DBDatabaseNotAvailable message) -> do
      addFlash (OperationFailed, message)
      getHomeOrUploadLink
    Left _ -> internalError
    Right document -> do
      postDocumentRejectedChange document siglinkid "web"
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
  invitedlink <- guardJust $ getSigLinkFor document signatorylinkid
  switchLocaleWhenNeeded  (Just invitedlink) document
  _ <- dbUpdate $ MarkDocumentSeen documentid signatorylinkid magichash
       (signatoryActor ctxtime ctxipnumber (maybesignatory invitedlink) (getEmail invitedlink) signatorylinkid)
  _ <- addSignStatLinkEvent document invitedlink

  ctx <- getContext
  content <- pageDocumentSignView ctx document invitedlink
  simpleResponse content

{- |
   Handles the request to show a document to a logged in user.
   URL: /d/{documentid}
   Method: GET
 -}
handleIssueShowGet :: Kontrakcja m => DocumentID -> m (Either KontraLink (Either Response String))
handleIssueShowGet docid = checkUserTOSGet $ do
  document <- guardRightM $ getDocByDocID docid
  muser <- ctxmaybeuser <$> getContext
  mcompany <- ctxcompany <$> getContext

  let mMismatchMessage = getDataMismatchMessage $ documentcancelationreason document
  when (isAuthor (document, muser) && isCanceled document && isJust mMismatchMessage) $
    addFlash (OperationFailed, fromJust mMismatchMessage)

  authorsiglink <- guardJust $ getAuthorSigLink document
  let ispreparation = documentstatus document == Preparation
      isauthor = (userid <$> muser) == maybesignatory authorsiglink
      isincompany = isJust (maybecompany authorsiglink) &&
                      ((usercompany =<< muser) == maybecompany authorsiglink || 
                       (companyid <$> mcompany) == maybecompany authorsiglink)
      isauthororincompany = isauthor || isincompany
      isattachment = isAttachment document
      msiglink = find (isSigLinkFor muser) $ documentsignatorylinks document
      
      isadmin = Just True == (useriscompanyadmin <$> muser)
      iscompany = hasSigLinkFor (usercompany =<< muser) document
      isadminofcompany = isadmin && iscompany

  ctx <- getContext
  case (ispreparation, msiglink) of
    (True,  _) | isattachment        -> Right <$> pageAttachmentDesign document
    (True,  _)                       -> Right <$> pageDocumentDesign document
    (False, _) | isauthororincompany || isadminofcompany -> Right <$> pageDocumentView document msiglink (isadminofcompany || isincompany)
    (False, Just siglink)            -> Left  <$> (simpleResponse =<< pageDocumentSignView ctx document siglink)
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
handleIssueShowPost :: Kontrakcja m => DocumentID -> m (Either KontraLink JSValue)
handleIssueShowPost docid = do
  document <- guardRightM $ getDocByDocID docid
  Context { ctxmaybeuser = muser } <- getContext
  unless (isAuthor (document, muser)) internalError -- still need this because others can read document
  sign              <- isFieldSet "sign"
  send              <- isFieldSet "send"
  -- Behold!
  case documentstatus document of
    Preparation | sign              -> Right <$> (linkAsJSON $ handleIssueSign document)
    Preparation | send              -> Right <$> (linkAsJSON $ handleIssueSend document)
    _ | canAuthorSignLast document  -> Left  <$> handleIssueSignByAuthor document
    _ -> return $ Left LinkArchive
 where
     linkAsJSON :: (Kontrakcja m) => m KontraLink -> m JSValue
     linkAsJSON lg = do
         l <- lg
         runJSONGenT $ J.value "link" (show l)

handleIssueSign :: Kontrakcja m => Document -> m KontraLink
handleIssueSign document = do
    Log.debug "handleIssueSign"
    ctx <- getContext
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM forIndividual docs
        case partitionEithers mndocs of
          ([], [d]) -> do
            when_ (sendMailsDurringSigning d) $ do
                addFlashM $ modalSendConfirmationView d True
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
                Signable _ -> return $ LinkArchive
                _          -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSign had lefts: " ++ intercalate ";" ls
            addFlash (OperationFailed, intercalate ";" ls)
            return $ LinkIssueDoc (documentid document)
      Left link -> return link
    where
      forIndividual :: Kontrakcja m => Document -> m (Either String Document)
      forIndividual doc = do
        Log.debug $ "handleIssueSign for forIndividual " ++ show (documentid doc)  
        mprovider <- readField "eleg"
        mndoc <- case mprovider of
                   Nothing ->  Right <$> authorSignDocument (documentid doc) Nothing
                   Just provider -> do
                     transactionid <- getDataFnM $ look "transactionid"
                     esigninfo <- case provider of
                       MobileBankIDProvider -> do
                         BankID.verifySignatureAndGetSignInfoMobileForAuthor (documentid doc) transactionid
                       _ -> do
                         signature <- getDataFnM $ look "signature"
                         BankID.verifySignatureAndGetSignInfoForAuthor (documentid doc) provider signature transactionid
                     case esigninfo of
                       BankID.Problem msg -> return $ Left msg
                       BankID.Mismatch msg _ _ _ -> do
                         Log.debug $ "got this message: " ++ msg
                         return $ Left msg
                       BankID.Sign sinfo -> Right <$>  authorSignDocument (documentid doc) (Just sinfo)
        case mndoc of
          Right (Right newdocument) -> do
            postDocumentPreparationChange newdocument "web"
            newdocument' <- guardJustM $ dbQuery $ GetDocumentByDocumentID (documentid newdocument)
            postDocumentPendingChange newdocument' newdocument' "web" -- | We call it on same document since there was no change
            return $ Right newdocument'
          Right (Left (DBActionNotAvailable message)) -> return $ Left message
          Right (Left (DBDatabaseNotAvailable message)) -> return $ Left message
          Right (Left _) -> return $ Left "Server error. Please try again."
          Left s -> return $ Left s


handleIssueSend :: Kontrakcja m => Document -> m KontraLink
handleIssueSend document = do
    Log.debug "handleIssueSend"
    ctx <- getContext
    mdocs <- splitUpDocument document
    case mdocs of
      Right docs -> do
        mndocs <- mapM forIndividual docs
        case partitionEithers mndocs of
          ([], [d]) -> do
            when_ (sendMailsDurringSigning d) $ do
                addFlashM $ modalSendConfirmationView d False
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
                Signable _ -> return $ LinkArchive
                _ -> return $ LinkUpload
          (ls, _) -> do
            Log.debug $ "handleIssueSend had lefts: " ++ intercalate ";" (map show ls)
            internalError
      Left link -> return link
    where
      forIndividual doc = do
        Log.debug $ "handleIssueSign for forIndividual " ++ show (documentid doc)
        mndoc <- authorSendDocument (documentid doc)
        case mndoc of
          Right newdocument -> postDocumentPreparationChange newdocument "web"
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
          dbUpdate $ DocumentFromSignatoryData (documentid udoc) (item 0) (item 1) (item 2) (item 3) (item 4) (item 5) (drop 6 xs) actor
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
             postDocumentPendingChange ndoc doc "web"
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
   contents <- getFileIDContents fileid'
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
  let pending JpegPagesPending = True
      pending _                = False
      files                    = map authorattachmentfile (documentauthorattachments doc)
  case files of
    [] -> return $ toResponse ""
    f  -> do
      b <- mapM maybeScheduleRendering f
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
      b <- mapM maybeScheduleRendering f
      if any pending b
        then notFound (toResponse "temporary unavailable (document has files pending for process)")
        else do
        pages <- Doc.DocView.showFilesImages2 (documentid doc) Nothing $ zip f b
        simpleResponse pages

{- We return pending message if file is still pending, else we return JSON with number of pages-}
handleFilePages :: Kontrakcja m => FileID -> m JSValue
handleFilePages fid = do
  -- Here we need to implement access rights, this will go as follows:
  --
  -- If we have documentid then we look for logged in user and
  -- signatorylinkid and magichash (in cookie). Then we check if file is
  -- reachable as document file, document sealed file, document author
  -- attachment or document signatory attachment.
  --
  -- If we have attachmentid then we look for logged in user and see
  -- if user owns the file or file is shared in user's company.
  --
  -- URLs look like:
  -- /filepages/#fileid/This%20is%file.pdf?documentid=34134124
  -- /filepages/#fileid/This%20is%file.pdf?documentid=34134124&signatorylinkid=412413
  -- /filepages/#fileid/This%20is%file.pdf?attachmentid=34134124
  --
  -- Warning take into account when somebody has saved document into
  -- hers account but we still refer using signatorylinkid.
  --
  -- Exactly same access rights should apply to file download and to individual pages.
  jpages <- maybeScheduleRendering fid
  runJSONGenT $ case jpages of
    JpegPagesPending -> do
      J.value "wait" "Rendering in progress"
    JpegPagesError _ -> do
      J.value "error" "Rendering failed"
    JpegPages pages  -> do
      J.objects "pages" $ for pages $ \(_,width,height) -> do
        J.value "width"  width
        J.value "height" height

handlePageOfDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink Response)
handlePageOfDocument docid = checkUserTOSGet $ handlePageOfDocument' docid Nothing

handlePageOfDocumentForSignatory :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> m Response
handlePageOfDocumentForSignatory docid siglinkid sigmagichash = do
    doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
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
             b <- mapM maybeScheduleRendering f
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
        _ <- addDocumentCreateStatEvents doc "web"
        return $ LinkIssueDoc $ documentid doc

handleCreateNewTemplate:: Kontrakcja m => m KontraLink
handleCreateNewTemplate = withUserPost $ do
  input <- getDataFnM (lookInput "doc")
  docprocess <- fromMaybe Contract `fmap` getDocProcess
  mdoc <- makeDocumentFromFile (Template docprocess) input 1
  case mdoc of
    Nothing -> return $ LinkArchive
    Just doc -> do
      _ <- addDocumentCreateStatEvents doc "web"
      return $ LinkIssueDoc $ documentid doc

handleCreateNewAttachment:: Kontrakcja m => m JSValue
handleCreateNewAttachment = do
  guardLoggedIn
  input <- getDataFnM (lookInput "doc")
  mdoc <- makeDocumentFromFile Attachment input 0
  when_ (isJust mdoc) $ addDocumentCreateStatEvents (fromJust mdoc) "web"
  J.runJSONGenT $ return ()

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

handleAttachmentRename :: Kontrakcja m => DocumentID -> m KontraLink
handleAttachmentRename docid = withUserPost $ do
  newname <- getCriticalField return "docname"
  actor <- guardJustM $ mkAuthorActor <$> getContext
  doc <- guardRightM $ dbUpdate $ SetDocumentTitle docid newname actor
  return $ LinkIssueDoc $ documentid doc

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
    iprev <- preview fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid Nothing fileid

showPreviewForSignatory:: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> FileID -> m (Either KontraLink Response)
showPreviewForSignatory docid siglinkid sigmagichash fileid = do
    doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
    checkLinkIDAndMagicHash doc siglinkid sigmagichash
    iprev <- preview fileid 0
    case iprev of
         Just res -> return $ Right res
         Nothing ->   return $ Left $ LinkDocumentPreview docid (getMaybeSignatoryLink (doc,siglinkid)) fileid

preview :: Kontrakcja m => FileID -> Int -> m (Maybe Response)
preview fid value
  | value > 10 = return Nothing
  | otherwise  =   do
        Context{ctxnormalizeddocuments} <- getContext
        jpages <- MemCache.get fid ctxnormalizeddocuments
        case jpages of
            Just (JpegPages pages) -> do
                let (contents,_,_) =  pages !! 0
                scaledContent <- liftIO $ scaleForPreview contents
                let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [scaledContent]) Nothing
                return $ Just $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/jpeg") res
            other -> do
                when_ (other == Nothing) $ maybeScheduleRendering fid
                liftIO $ threadDelay 500000
                preview fid (value+1)


showPage' :: Kontrakcja m => FileID -> Int -> m Response
showPage' fileid pageno = do
  Context{ctxnormalizeddocuments} <- getContext
  jpages <- MemCache.get fileid ctxnormalizeddocuments
  case jpages of
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
  if isPending doc
    then do
    mdoc' <- dbUpdate $ CancelDocument (documentid doc) ManualCancel actor
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
  mdoc <- dbQuery $ GetDocumentByDocumentID docid
  case (mdoc) of
    Just doc -> withDocumentAutho doc $ do
                          dbUpdate $ WithdrawnDocument $ documentid doc
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
  ctx <- getContext
  doc <- guardRightM $ getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid
  signlink <- guardJust $ getSigLinkFor doc signlinkid
  customMessage <- getCustomTextField "customtext"
  _ <- sendReminderEmail customMessage ctx doc signlink
  addFlashM $ flashRemindMailSent signlink
  return (LinkIssueDoc docid)

--This only works for undelivered mails. We shoulkd check if current user is author
handleChangeSignatoryEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleChangeSignatoryEmail docid slid = withUserPost $ do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Just email -> do
      edoc <- getDocByDocIDForAuthor docid
      case edoc of
        Left _ -> return LoopBack
        Right doc -> do
          muser <- dbQuery $ GetUserByEmail (documentservice doc) (Email email)
          actor <- guardJustM $ mkAuthorActor <$> getContext
          mnewdoc <- dbUpdate $ ChangeSignatoryEmailWhenUndelivered docid slid muser email actor
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
handleShowUploadPage = checkUserTOSGet $ uploadPage

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
      document <- guardJustM $ dbQuery $ GetDocumentByDocumentID $ did
      Log.debug $ show "Matching document found"
      auid <- guardJust $ join $ maybesignatory <$> getAuthorSigLink document
      auser <- guardJustM $ dbQuery $ GetUserByID auid
      let haspermission = (userid auser == userid user) ||
                          ((usercompany auser == usercompany user && (isJust $ usercompany user)) &&  isDocumentShared document)
      enewdoc <- if (isTemplate document && haspermission)
                    then do
                      Log.debug $ show "Valid persmision to create from template"
                      mcompany <- getCompanyForUser user
                      actor <- guardJustM $ mkAuthorActor <$> getContext
                      dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor
                    else internalError
      case enewdoc of
        Right newdoc -> do
          _ <- addDocumentCreateStatEvents newdoc "web"
          Log.debug $ show "Document created from template"
          return $ LinkIssueDoc $ documentid newdoc
        Left _ -> internalError
    Nothing -> internalError

-- | This handler downloads a file by file id. As specified in
-- handlePageOfDocument rules of access need to be obeyd. This handler
-- download file as is. If you need preprinted fields use
-- 'handleDownloadMainFile'.
handleDownloadFile :: Kontrakcja m => FileID -> String -> m Response
handleDownloadFile fid _nameForBrowser = do
  content <- getFileIDContents fid
  respondWithPDF content
  where
    respondWithPDF contents = do
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
      return res2

-- | This handler downloads main file of document. This means original
-- file for document when status is different than Closed or Pending,
-- preprinted fields document for Pending documents and sealed file
-- for Closed documents.
--
-- Use 'handleDownloadFile' to get file contents as is and to download
-- files different than main file.
handleDownloadMainFile :: Kontrakcja m => DocumentID -> String -> m Response
handleDownloadMainFile did _nameForBrowser = do
  msid <- readField "signatorylinkid"
  mmh <- readField "magichash"
  doc <- case (msid, mmh) of
           (Just sid, Just mh) -> guardRightM $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
           _ ->                   guardRightM $ getDocByDocID did

  content <- case documentstatus doc of
               Pending -> do
                 sourceFile <- guardJustM $ dbQuery $ GetFileByFileID (head $ documentfiles doc)
                 guardRightM $ DocSeal.presealDocumentFile doc sourceFile
               Closed -> do
                 -- Here we should actually respond with a redirect
                 -- that waits for file to appear. Hopefully nobody
                 -- clicks download that fast.
                 getFileIDContents (head $ documentsealedfiles doc)
               _ -> do
                 getFileIDContents (head $ documentfiles doc)
  respondWithPDF content
  where
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
  _ <- dbUpdate $ DeleteSigAttachment docid siglinkid fid
       (signatoryActor ctxtime ctxipnumber (maybesignatory siglink) email siglinkid)
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
  file <- dbUpdate $ NewFile attachname content
  let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory siglink) email siglinkid
  d <- guardRightM $ dbUpdate $ SaveSigAttachment docid siglinkid attachname (fileid file) actor
  return $ LinkSignDoc d siglink

jsonDocument :: Kontrakcja m => DocumentID -> m JSValue
jsonDocument did = do
    ctx <- getContext
    (mdoc,msiglink) <- jsonDocumentGetterWithPermissionCheck did
    pg <- case (userid <$> ctxmaybeuser ctx) of 
               Just uid ->  dbQuery $ GetPadQueue uid
               Nothing -> return Nothing
    when_ (isJust mdoc && isJust msiglink) $ do 
         let sl = fromJust msiglink 
         dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
              (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
    cttime <- liftIO $ getMinutesTime
    case mdoc of
         Nothing -> return $ JSObject $ toJSObject [("error",JSString $ toJSString "No document avaible")]
         Just doc -> do
             switchLocaleWhenNeeded msiglink doc
             documentJSON pg msiglink cttime doc

jsonDocumentGetterWithPermissionCheck ::   Kontrakcja m => DocumentID -> m (Maybe Document, Maybe SignatoryLink)
jsonDocumentGetterWithPermissionCheck did = do
    ctx <- getContext
    mmagichashh <- readField "magichash"
    msignatorylink <- readField "signatoryid"
    case (msignatorylink,mmagichashh) of
        (Just slid,Just mh) -> do
                   mdoc <- dbQuery $ GetDocumentByDocumentID did
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
  docs <- dbQuery $ GetDocumentsByService Nothing
  let probs = listInvariantProblems ctxtime docs
      res = case probs of
        [] -> "No problems!"
        _  -> intercalate "\n" probs
  return $ Response 200 Map.empty nullRsFlags (BSL.fromString res) Nothing



prepareEmailPreview :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m JSValue
prepareEmailPreview docid slid = do
    mailtype <- getField' "mailtype"
    doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
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
    doc <- guardRightM $ getDocByDocIDForAuthor did
    draftData <- guardJustM $ do
        v <- liftM (runGetJSON readJSObject) $ getField' "draft"
        case v of
         Right j -> return $ fromJSValue j
         _ -> return Nothing
    actor     <- guardJustM $ mkAuthorActor <$> getContext
    res       <- applyDraftDataToDocument doc draftData actor
    case res of
         Right _ -> return $ JSObject $ toJSObject []
         Left s -> do
             return $ JSObject $ toJSObject [("error",JSString $ toJSString $ "Document saving failed with ("++s++") - unless someone is experimenting this should never happend")]


handleSetAttachments :: Kontrakcja m => DocumentID -> m KontraLink
handleSetAttachments did = do
    doc <- guardRightM $ getDocByDocIDForAuthor did
    attachments <- getAttachments 0
    Log.debug $ "Setting attachments to " ++ show attachments
    actor <- guardJustM $ mkAuthorActor <$> getContext
    forM_ (documentauthorattachments doc) $ \att -> dbUpdate $ RemoveDocumentAttachment did (authorattachmentfile att) actor
    forM_ (nub attachments) $ \att -> dbUpdate $ AddDocumentAttachment did att actor -- usage of nub is ok, as we never expect this list to be big
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
                          Just fid -> (fmap fileid) <$> (dbQuery $ GetFileByFileID fid)
                          Nothing -> return $ Nothing
                 _ -> return Nothing

handleParseCSV :: Kontrakcja m => m JSValue
handleParseCSV = do
  ctx <- getContext
  _ <- guardJust $ ctxmaybeuser ctx
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
                 Left _ -> oneProblemJSON $ renderTemplate_ "flashMessageFailedToParseCSV"
                 Right contents
                   | length contents > 1000 -> oneProblemJSON $ renderTemplate "flashMessageCSVHasTooManyRows" $ F.value "maxrows" (1000::Int)
                   | otherwise -> do
                       let (problems, csvdata) = cleanCSVContents eleg customfieldscount contents
                       runJSONGenT $ do
                         J.objects "problems" $ for problems $ \p -> do
                           J.valueM "description" $ csvProblemToDescription p
                           when (isJust $ problemRow p) $
                             J.value "row" $ fromJust $ problemRow p
                           when (isJust $ problemCell p) $
                             J.value "cell" $ fromJust $ problemCell p
                         J.value "rows" $ csvbody csvdata

        _ -> do
            oneProblemJSON $ renderTemplate_ "flashMessageFailedToParseCSV"
  return res
  where
      oneProblemJSON :: Kontrakcja m => m String -> m JSValue
      oneProblemJSON desc = runJSONGenT $ do
        J.object "problems" $ do
          J.valueM "description" desc
        J.value "rows" ([]::[String])

-- | Switch to document language. Checks first if there is not logged in user. If so it will switch only if this is a different signatory.        
switchLocaleWhenNeeded :: (Kontrakcja m) => Maybe SignatoryLink -> Document -> m ()
switchLocaleWhenNeeded mslid doc = do
  cu <- ctxmaybeuser <$> getContext
  when (isNothing cu || ((isJust mslid) && not (isSigLinkFor cu mslid))) $ switchLocale (getLocale doc)



-- GuardTime verification page. This can't be external since its a page in our system.


handleShowVerificationPage :: Kontrakcja m =>  m String
handleShowVerificationPage = gtVerificationPage


handleVerify :: Kontrakcja m => m JSValue
handleVerify = do
      fileinput <- getDataFn' (lookInput "file")
      filepath <- case fileinput of
            Just (Input (Left filepath) _ _) -> return filepath
            Just (Input (Right content) _ _) -> liftIO $ do
                    systmp <- getTemporaryDirectory
                    (pth,handle) <- openTempFile systmp ("vpath.pdf")
                    BSL.hPutStr handle content
                    return pth
            _ -> internalError        
      liftIO $ toJSValue <$> GuardTime.verify filepath

                    
