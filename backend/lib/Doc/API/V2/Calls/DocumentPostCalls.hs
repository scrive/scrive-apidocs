module Doc.API.V2.Calls.DocumentPostCalls (
  docApiV2New
, docApiV2NewFromTemplate
, docApiV2Update
, docApiV2Start
, docApiV2Prolong
, docApiV2Cancel
, docApiV2Trash
, docApiV2Delete
, docApiV2TrashMultiple
, docApiV2DeleteMultiple
, docApiV2Remind
, docApiV2Forward
, docApiV2SetFile
, docApiV2SetAttachments
, docApiV2SetAutoReminder
, docApiV2Clone
, docApiV2Restart
, docApiV2RemovePages
, docApiV2Callback
, docApiV2SetSharing
, docApiV2SigSetAuthenticationToView
, docApiV2SigSetAuthenticationToViewArchived
, docApiV2SigSetAuthenticationToSign
, docApiV2SigChangeEmailAndMobile
, docApiV2GenerateShareableLink
, docApiV2DiscardShareableLink
, docApiV2AddImage
) where

import Control.Monad.Base
import Crypto.RNG
import Data.Unjson
import Data.Unjson as Unjson
import Happstack.Server.Types
import Log
import System.FilePath (dropExtension)
import Text.StringTemplates.Templates
import qualified Data.Text as T

import API.V2
import API.V2.Parameters
import Attachment.Model
import Chargeable.Model
import DB
import DB.TimeZoneName (defaultTimeZoneName)
import Doc.Action
import Doc.Anchors
import Doc.API.Callback.Model (triggerAPICallbackIfThereIsOne)
import Doc.API.V2.DocumentAccess
import Doc.API.V2.DocumentUpdateUtils
import Doc.API.V2.Guards
import Doc.API.V2.JSON.AttachmentDetails
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.Misc
import Doc.AutomaticReminder.Model (setAutomaticReminder)
import Doc.DocAddImage (addImageToDocumentFile)
import Doc.DocInfo (isTimedout)
import Doc.DocMails (sendAllReminderEmailsExceptAuthor, sendForwardEmail, sendInvitationEmail1)
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import File.File (File(..))
import File.Storage
import InputValidation (asValidEmail, asValidPhone)
import Kontra
import Log.Identifier
import MinutesTime
import OAuth.Model
import User.Email (Email(..))
import User.Model
import Util.Actor (Actor)
import Util.HasSomeUserInfo (getEmail, getMobile)
import Util.PDFUtil
import Util.SignatoryLinkUtils (getAuthorSigLink, getSigLinkFor, isAuthor)

docApiV2New :: Kontrakcja m => m Response
docApiV2New = api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  -- Parameters
  saved <- apiV2ParameterDefault True (ApiV2ParameterBool "saved")
  mFile <- apiV2ParameterOptional (ApiV2ParameterFilePDF "file")
  -- API call actions
  title <- case mFile of
    Nothing -> do
      ctx <- getContext
      title <- renderTemplate_ "newDocumentTitle"
      return $ title ++ " " ++ formatTimeSimple (get ctxtime ctx)
    Just f -> return . dropExtension . filename $ f
  (dbUpdate $ NewDocument user title Signable defaultTimeZoneName 0 actor) `withDocumentM` do
    dbUpdate $ SetDocumentUnsavedDraft (not saved)
    case mFile of
      Nothing -> return ()
      Just f -> do
        dbUpdate $ AttachFile (fileid f) actor
  -- Result
    theDocument >>= \doc -> logInfo "New document created" $ logObject_ doc
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2NewFromTemplate :: Kontrakcja m => DocumentID -> m Response
docApiV2NewFromTemplate did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  -- Guards
  withDocumentID did $ do
    guardThatUserIsAuthorOrDocumentIsShared user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (isTemplate) "The document is not a template." =<< theDocument
    guardThatDocumentIs (not $ flip documentDeletedForUser $ userid user) "The template is in Trash" =<< theDocument
  -- API call actions
  template <- dbQuery $ GetDocumentByDocumentID $ did
  (apiGuardJustM (serverError "Can't clone given document") (dbUpdate $ CloneDocumentWithUpdatedAuthor (Just user) template actor id) >>=) $ flip withDocumentID $ do
    dbUpdate $ DocumentFromTemplate (documentid template) actor
    dbUpdate $ SetDocumentUnsavedDraft False
  -- Result
    newDoc <- theDocument
    logInfo "New document created from template" $ object [
        logPair ("new_"<>)      newDoc
      , logPair ("template_"<>) template
      ]
    return $ Created (unjsonDocument $ documentAccessForUser user newDoc, newDoc)


docApiV2Update :: Kontrakcja m => DocumentID -> m Response
docApiV2Update did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument
    -- Parameters
    documentJSON <- apiV2ParameterObligatory (ApiV2ParameterAeson "document")
    doc <- theDocument
    let da = documentAccessForUser user doc
    draftData <- case (Unjson.update doc (unjsonDocument da) documentJSON) of
      (Result draftData []) -> do
        guardThatConsentModulesAreOnSigningParties draftData
        return draftData
      (Result _ errs) ->
        apiError $ requestParameterParseError "document" $ "Errors while parsing document data:" <+> T.pack (show errs)
    -- API call actions
    applyDraftDataToDocument draftData actor
    guardThatAuthorIsNotApprover =<< theDocument
    -- Result
    Ok <$> (unjsonDocument da,) <$> theDocument


docApiV2Start :: Kontrakcja m => DocumentID -> m Response
docApiV2Start did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument
    guardThatDocumentCanBeStarted =<< theDocument
    -- Parameters
    authorSignsNow <- apiV2ParameterDefault False (ApiV2ParameterBool "author_signs_now")
    t <- get ctxtime <$> getContext
    timezone <- documenttimezonename <$> theDocument
    clearDocFields actor
    dbUpdate $ PreparationToPending actor timezone
    dbUpdate $ SetDocumentInviteTime t actor
    postDocumentPreparationChange authorSignsNow timezone
    dbUpdate $ ChargeUserGroupForStartingDocument did
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Prolong :: Kontrakcja m => DocumentID -> m Response
docApiV2Prolong did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (isTimedout)
        "The document has not timed out. Only timed out documents can be prolonged."  =<< theDocument
    -- Parameters
    days <- fromIntegral <$> apiV2ParameterObligatory (ApiV2ParameterInt "days")
    when (days < 1 || days > 365) $
      apiError $ requestParameterInvalid "days" "Days must be a number between 1 and 365"
    -- API call actions
    timezone <- documenttimezonename <$> theDocument
    dbUpdate $ ProlongDocument days timezone actor
    triggerAPICallbackIfThereIsOne =<< theDocument
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Cancel :: Kontrakcja m => DocumentID -> m Response
docApiV2Cancel did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    -- API call actions
    dbUpdate $ CancelDocument actor
    postDocumentCanceledChange =<< theDocument
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Trash :: Kontrakcja m => DocumentID -> m Response
docApiV2Trash = docApiV2TrashDeleteCommon ArchiveDocument


docApiV2Delete :: Kontrakcja m => DocumentID -> m Response
docApiV2Delete = docApiV2TrashDeleteCommon ReallyDeleteDocument


docApiV2TrashDeleteCommon
  :: (Kontrakcja m, DBUpdate (DocumentT m) t b)
  => (UserID -> Actor -> t)
  -> DocumentID
  -> m Response
docApiV2TrashDeleteCommon dbAction did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  -- Guards
  guardThatDocumentCanBeTrashedOrDeletedByUser user did
  withDocumentID did $ do
    -- API call actions
    void . dbUpdate $ dbAction (userid user) actor
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2TrashMultiple :: Kontrakcja m => m Response
docApiV2TrashMultiple = docApiV2TrashDeleteMultipleCommon ArchiveDocument


docApiV2DeleteMultiple :: Kontrakcja m => m Response
docApiV2DeleteMultiple = docApiV2TrashDeleteMultipleCommon ReallyDeleteDocument


docApiV2TrashDeleteMultipleCommon
  :: (Kontrakcja m, DBUpdate (DocumentT m) t b)
  => (UserID -> Actor -> t)
  -> m Response
docApiV2TrashDeleteMultipleCommon dbAction = api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  -- Parameters
  dids <- apiV2ParameterObligatory $ ApiV2ParameterJSON "document_ids"
            $ arrayOf unjsonDef
  -- Guards
  when (length dids > 100)
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't have more than 100 positions"
  when (length dids == 0)
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't be an empty list"
  when (length (nub dids) /= length dids )
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't contain duplicates"
  forM_  dids $ guardThatDocumentCanBeTrashedOrDeletedByUser user
  -- API call actions
  forM_ dids $ \did -> withDocumentID did
    . dbUpdate $ dbAction (userid user) actor
  -- Result
  let docDomain = DocumentsVisibleToUser $ userid user
      docFilter = [DocumentFilterByDocumentIDs dids]
      limits    = (0, 100, 100)
      docQuery  = GetDocumentsWithSoftLimit docDomain docFilter [] limits
      docAccess = \d -> (documentAccessForUser user d, d)
      headers   = mkHeaders [("Content-Type", "application/json; charset=UTF-8")]
  (docCount, allDocs) <- dbQuery $ docQuery
  let jsonBS = listToJSONBS (docCount, docAccess <$> allDocs)
  return . Ok $ Response 200 headers nullRsFlags jsonBS Nothing


docApiV2Remind :: Kontrakcja m => DocumentID -> m Response
docApiV2Remind did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    -- API call actions
    void $ sendAllReminderEmailsExceptAuthor actor False
    -- Result
    return $ Accepted ()


docApiV2Forward :: Kontrakcja m => DocumentID -> m Response
docApiV2Forward did = logDocument did . api $ do
  -- Permissions
  (user,_) <- getAPIUser APIDocCheck
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    -- Make sure we only send out the document with the author's signatory link
    -- when it is closed, otherwise the link may be abused
    guardDocumentStatus Closed =<< theDocument
    -- Parameters
    validEmail <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterTextWithValidation "email" asValidEmail)
    noContent <- apiV2ParameterDefault True (ApiV2ParameterBool "no_content")
    noAttachments <- apiV2ParameterDefault False (ApiV2ParameterBool "no_attachments")
    -- API call actions
    asiglink <- fromJust <$> getAuthorSigLink <$> theDocument
    void $ sendForwardEmail validEmail noContent noAttachments asiglink
    -- Return
    return $ Accepted ()


docApiV2SetFile :: Kontrakcja m => DocumentID -> m Response
docApiV2SetFile did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument
    -- Parameters
    mFile <- apiV2ParameterOptional (ApiV2ParameterFilePDF "file")
    -- API call actions
    case mFile of
      Nothing -> dbUpdate $ DetachFile actor
      Just file -> do
        moldfileid <- fmap mainfileid <$> documentfile <$> theDocument
        dbUpdate $ AttachFile (fileid file) actor
        case moldfileid of
          Just oldfileid -> recalculateAnchoredFieldPlacements oldfileid (fileid file)
          Nothing -> return ()
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2RemovePages :: Kontrakcja m => DocumentID -> m Response
docApiV2RemovePages did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument

    -- Parameters
    pages <- apiV2ParameterObligatory (ApiV2ParameterAeson "pages")

    when (length pages > 100) $ do
      apiError $ requestParameterInvalid "pages" "Pages parameter can't have more than 100 positions"
    when (length pages == 0) $
      apiError $ requestParameterInvalid "pages" "Pages parameter can't be an empty list"
    when (length (nub pages) /= length pages ) $
      apiError $ requestParameterInvalid "pages" "Pages parameter can't contain duplicates"

    -- Generating and replacing PDF
    mfile <- fileFromMainFile =<< documentfile <$> theDocument
    case mfile of
      Nothing -> apiError $ documentStateError "Document does not have a main file"
      Just file -> do
        content <- getFileContents file
        enop <- liftBase $ getNumberOfPDFPages content
        case enop of
          Left _ -> apiError $ serverError "Can't extract number of pages from PDF"
          Right nop -> do
            when (any (\p -> p > nop || p < 1) pages) $ do
              apiError $ requestParameterInvalid "pages" "Some page indexes lower then 1 or higher then number of pages"

            let pagesToPick = [1..nop]  \\ pages
            case pagesToPick of
              [] -> apiError $ requestParameterInvalid "pages" "Can't remove all pages from PDF"
              _ -> do
                mnewcontent <- pickPages pagesToPick content
                case mnewcontent of
                  Nothing -> apiError $ serverError "Removing pages from file failed"
                  Just newcontent -> do
                    nfileid <- saveNewFile (filename file) newcontent
                    dbUpdate $ DetachFile actor
                    dbUpdate $ AttachFile nfileid actor

    -- Changing signatory fiels
    adjustFieldAndPlacementsAfterRemovingPages pages actor

    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument



docApiV2SetAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2SetAttachments did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument
    -- Parameters
    attachmentDetails <- apiV2ParameterObligatory (ApiV2ParameterJSON "attachments" $ arrayOf unjsonAttachmentDetails)
    guardThatAttachmentDetailsAreConsistent attachmentDetails
    incremental <- apiV2ParameterDefault False (ApiV2ParameterBool "incremental")

    -- We fetch a function for checking if attachment was part of document before call. This has to be here - since next step is purging all attachments.
    fileWasAlreadyAnAttachment <- theDocument >>= (\d -> return $ \fid -> fid `elem` (authorattachmentfileid <$> documentauthorattachments d))

    let names = map aadName attachmentDetails
    (documentauthorattachments <$> theDocument >>=) $ mapM_ $ \att ->
      unless (incremental && T.pack (authorattachmentname att) `notElem` names) $
        void $ dbUpdate $ RemoveDocumentAttachments (authorattachmentfileid att) actor

    newFileContentsWithDetails' <- forM attachmentDetails $ \ad ->  case (aadFileOrFileParam ad) of
      Left fid -> do
        attachmentFromAttachmentArchive <- (not null) <$> dbQuery (attachmentsQueryFor user fid)
        when (not (fileWasAlreadyAnAttachment fid || attachmentFromAttachmentArchive)) $
            apiError $ resourceNotFound $ "File id" <+> (T.pack . show $ fid)
              <+> "can't be used. It may not exist or you don't have permission to use it."
        void $ dbUpdate $ AddDocumentAttachment (aadName ad) (aadRequired ad) (aadAddToSealedFile ad) fid actor
        return Nothing
      Right fp -> return $ Just (ad, fp)
    let newFileContentsWithDetails = catMaybes newFileContentsWithDetails'
        newFileContents = map snd newFileContentsWithDetails
    newFiles <- apiV2ParameterObligatory $ ApiV2ParameterFilePDFs newFileContents
    let newFilesWithDetails = zip (map fst newFileContentsWithDetails) newFiles
    forM_ newFilesWithDetails $ \(ad, newFile) -> dbUpdate $ AddDocumentAttachment (aadName ad) (aadRequired ad) (aadAddToSealedFile ad) (fileid newFile) actor
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument

  where
    attachmentsQueryFor user fid = GetAttachments [ AttachmentsSharedInUsersUserGroup (userid user)
                                                  , AttachmentsOfAuthorDeleteValue  (userid user) True
                                                  , AttachmentsOfAuthorDeleteValue  (userid user) False
                                                  ]
                                                  [AttachmentFilterByFileID fid]
                                                  []

docApiV2SetAutoReminder :: Kontrakcja m => DocumentID -> m Response
docApiV2SetAutoReminder did = logDocument did . api $ do
  -- Permissions
  (user,_) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    -- Parameters
    daysParam <- apiV2ParameterOptional (ApiV2ParameterInt "days")
    days <- case daysParam of
      Nothing -> return Nothing
      Just d -> do
        ctx <- getContext
        tot <- documenttimeouttime <$> theDocument
        if d < 1 || (isJust tot && d `daysAfter` (get ctxtime ctx) > fromJust tot)
          then apiError $ requestParameterInvalid "days" "Must be a number between 1 and the number of days left to sign"
          else return $ Just d
    -- API call actions
    timezone <- documenttimezonename <$> theDocument
    setAutomaticReminder did (fmap fromIntegral days) timezone
    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Clone :: Kontrakcja m => DocumentID -> m Response
docApiV2Clone did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    -- API call actions
    doc <- theDocument
    mNewDid <- dbUpdate $ CloneDocumentWithUpdatedAuthor (Just user) doc actor id
    when (isNothing mNewDid) $
      apiError $ serverError "Could not clone document, did not get back valid ID"
    newdoc <- dbQuery $ GetDocumentByDocumentID $ fromJust mNewDid
    -- Result
    logInfo "New document created by cloning" $ object [
        logPair_ newdoc
      , "parent doc id" .= show did
      ]
    return $ Created $ (\d -> (unjsonDocument $ documentAccessForUser user d,d)) newdoc


docApiV2Restart :: Kontrakcja m => DocumentID -> m Response
docApiV2Restart did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (\d -> not $ documentstatus d `elem` [Preparation, Pending, Closed])
      "Documents that are in Preparation, Pending, or Closed can not be restarted." =<< theDocument
    -- API call actions
    doc <- theDocument
    mNewDoc <- dbUpdate $ RestartDocument doc actor
    when (isNothing mNewDoc) $
      apiError $ serverError "Could not restart document"
    -- Result
    return $ Created $ (\d -> (unjsonDocument $ documentAccessForUser user d,d)) (fromJust mNewDoc)

docApiV2Callback :: Kontrakcja m => DocumentID -> m Response
docApiV2Callback did = logDocument did . api $ do
  -- Permissions
  (user, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (\d -> not $ documentstatus d == Preparation)
      "Can not send callbacks for documents in Preparation." =<< theDocument
    -- API call actions
    triggerAPICallbackIfThereIsOne =<< theDocument
    -- Return
    return $ Accepted ()

docApiV2SetSharing :: Kontrakcja m => m Response
docApiV2SetSharing = api $ do
  -- Permissions
  (user, _) <- getAPIUser APIDocCreate
  -- Parameters
  dids      <- apiV2ParameterObligatory $ ApiV2ParameterJSON "document_ids"
                $ arrayOf unjsonDef
  sharing   <- apiV2ParameterObligatory $ ApiV2ParameterBool "shared"
  -- Guards
  when (length dids > 100)
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't have more than 100 positions"
  when (length dids == 0)
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't be an empty list"
  when (length (nub dids) /= length dids )
    . apiError $ requestParameterInvalid "document_ids"
      "document_ids parameter can't contain duplicates"
  forM_ dids $ \did -> withDocumentID did $
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
  -- API call actions
  void . dbUpdate $ SetDocumentSharing dids sharing
  -- Return
  return $ Accepted ()

----------------------------------------

docApiV2SigSetAuthenticationToView
  :: Kontrakcja m
  => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthenticationToView =
  docApiV2SigSetAuthToView AuthenticationToView

docApiV2SigSetAuthenticationToViewArchived
  :: Kontrakcja m
  => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthenticationToViewArchived =
  docApiV2SigSetAuthToView AuthenticationToViewArchived

docApiV2SigSetAuthToView
  :: Kontrakcja m
  => AuthenticationKind -> DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthToView authKind did slid =
  logDocumentAndSignatory did slid . api $ do

  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user          =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending                       =<< theDocument
    guardSigningPartyHasNeitherSignedNorApproved slid =<< theDocument
    guardSignatoryHasNotIdentifiedToView slid         =<< theDocument
    -- Parameters
    authType <- apiV2ParameterObligatory
      (ApiV2ParameterTextUnjson "authentication_type"
       unjsonAuthenticationToViewMethod)
    mSSN_    <- (fmap T.unpack) <$>
                apiV2ParameterOptional (ApiV2ParameterText "personal_number")
    mMobile_ <- (fmap T.unpack) <$>
                apiV2ParameterOptional (ApiV2ParameterText "mobile_number")
    (mSSN, mMobile) <- case authType of
      StandardAuthenticationToView -> return (Nothing, Nothing)
      SMSPinAuthenticationToView   -> return (Nothing, mMobile_)
      SEBankIDAuthenticationToView -> return (mSSN_, Nothing)
      NOBankIDAuthenticationToView -> return (mSSN_, mMobile_)
      DKNemIDAuthenticationToView  -> return (mSSN_, Nothing)
      FITupasAuthenticationToView  -> return (mSSN_, Nothing)
    -- Check conditions on parameters and signatory
    guardCanSetAuthenticationToViewForSignatoryWithValues
      slid authKind authType mSSN mMobile
      =<< theDocument

    -- API call actions
    dbUpdate $
      ChangeAuthenticationToViewMethod slid authKind authType
      mSSN mMobile actor
    -- Return
    Ok <$>
      (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument

----------------------------------------

docApiV2SigSetAuthenticationToSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthenticationToSign did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    authentication_type <- apiV2ParameterObligatory (ApiV2ParameterTextUnjson "authentication_type" unjsonAuthenticationToSignMethod)
    mSSN_ <- (fmap T.unpack) <$> apiV2ParameterOptional (ApiV2ParameterText "personal_number")
    mMobile_ <- (fmap T.unpack) <$> apiV2ParameterOptional (ApiV2ParameterText "mobile_number")
    (mSSN, mMobile) <- case authentication_type of
      StandardAuthenticationToSign -> return (Nothing, Nothing)
      SEBankIDAuthenticationToSign -> return (mSSN_, Nothing)
      SMSPinAuthenticationToSign   -> return (Nothing, mMobile_)
      NOBankIDAuthenticationToSign -> return (Nothing, Nothing)
      DKNemIDAuthenticationToSign  -> return (mSSN_, Nothing)
    -- Check conditions on parameters and signatory
    guardCanSetAuthenticationToSignForSignatoryWithValue slid authentication_type mSSN mMobile =<< theDocument
    -- API call actions
    dbUpdate $ ChangeAuthenticationToSignMethod slid authentication_type mSSN mMobile actor
    -- Return
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument

docApiV2SigChangeEmailAndMobile :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigChangeEmailAndMobile did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    sl <- guardGetSignatoryFromIdForDocument slid
    when (isAuthor sl)
      (apiError $ signatoryStateError "Cannot change email or mobile of document author")
    -- Parameters
    -- We are not using `asValidPhoneForSMS`, as we might not need to be so
    -- strict. Instead `guardThatDocumentCanBeStarted` checks stuff later.
    validMobile <- apiV2ParameterOptional (ApiV2ParameterTextWithValidation "mobile_number" asValidPhone)
    validEmail <- apiV2ParameterOptional (ApiV2ParameterTextWithValidation "email" asValidEmail)
    -- Guard Parameters
    when (isNothing validMobile && isNothing validEmail ) (apiError $ requestParameterMissing "mobile_number or email")
    let hasMobileField = isJust . getFieldByIdentity MobileFI . signatoryfields $ sl
        hasEmailField  = isJust . getFieldByIdentity EmailFI  . signatoryfields $ sl
    when (isJust validMobile && not hasMobileField)
      (apiError $ signatoryStateError "Signatory has no mobile field, cannot set mobile number")
    when (isJust validEmail && not hasEmailField)
      (apiError $ signatoryStateError "Signatory has no email field, cannot set email")
    -- API call actions
    -- update mobile and email as per parameters
    case fmap T.unpack validMobile of
      Nothing -> return ()
      Just mobile ->
        when (getMobile sl /= mobile) (dbUpdate $ ChangeSignatoryPhone slid mobile actor)
    case fmap T.unpack validEmail of
      Nothing -> return ()
      Just email ->
        when (getEmail sl /= email) $ do
          emailUser <- dbQuery $ GetUserByEmail (Email email)
          dbUpdate $ ChangeSignatoryEmail slid emailUser email actor
    -- Once we've updated everything, the starting conditions should still be
    -- valid!
    -- This checks that email and mobile are valid for: invitation delivery,
    -- authentication to view and to sign
    guardThatDocumentCanBeStarted =<< theDocument
    -- When either of email or phone is changed, the magichash is regenerated,
    -- so we need a new SL from DB.
    sl' <- fromJust . getSigLinkFor slid <$> theDocument
    -- We always send both email and mobile invitations, even when nothing was changed.
    void $ sendInvitationEmail1 sl'
    -- API call actions
    Ok . (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument

docApiV2GenerateShareableLink :: Kontrakcja m => DocumentID -> m Response
docApiV2GenerateShareableLink did = logDocument did . api $ do
  (user, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    doc <- theDocument
    guardThatUserIsAuthor user doc
    guardThatDocumentIs isTemplate "The document is not a template." doc
    let doc' = doc { documenttype = Signable }
    guardThatDocumentCanBeStarted doc'

    hash <- random
    dbUpdate $ UpdateShareableLinkHash $ Just hash
    (\d -> Ok (unjsonDocument $ documentAccessForUser user d, d)) <$> theDocument

docApiV2DiscardShareableLink :: Kontrakcja m => DocumentID -> m Response
docApiV2DiscardShareableLink did = logDocument did . api $ do
  (user, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    guardThatUserIsAuthor user =<< theDocument
    guardThatDocumentIs isTemplate "The document is not a template." =<< theDocument
    dbUpdate $ UpdateShareableLinkHash Nothing
    return $ Accepted ()

docApiV2AddImage :: Kontrakcja m => DocumentID -> m Response
docApiV2AddImage did = logDocument did . api $ do
  -- Permissions
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    -- Guards
    guardThatUserIsAuthor user =<< theDocument
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation =<< theDocument

    -- Parameters
    image  <- apiV2ParameterObligatory (ApiV2ParameterBase64PNGImage "image")
    pageno <- apiV2ParameterObligatory (ApiV2ParameterInt "pageno")
    x      <- apiV2ParameterObligatory (ApiV2ParameterDouble "x")
    y      <- apiV2ParameterObligatory (ApiV2ParameterDouble "y")

    when (pageno < 1) $ do
      apiError $ requestParameterInvalid "pageno" "Page number should be >= 1"
    when (x < 0 || x > 1 || y < 0 || y > 1) $ do
      apiError $ requestParameterInvalid "x or y" "X and Y positions should be between 0 and 1"

    -- Generating and replacing PDF
    mfile <- fileFromMainFile =<< documentfile <$> theDocument
    case mfile of
      Nothing -> apiError $ documentStateError "Document does not have a main file"
      Just file -> do
        content <- getFileContents file
        enop <- liftBase $ getNumberOfPDFPages content
        case enop of
          Left _ -> apiError $ serverError "Can't extract number of pages from PDF"
          Right nop -> do
            when (pageno > nop) $ do
              apiError $ requestParameterInvalid "pageno" "Page index is higher than number of pages"

            mnewcontent <- addImageToDocumentFile did file image (fromIntegral pageno) x y
            case mnewcontent of
              Left _ -> apiError $ serverError "Adding image to main file has failed"
              Right newcontent -> do
                nfileid <- saveNewFile (filename file) newcontent
                dbUpdate $ DetachFile actor
                dbUpdate $ AttachFile nfileid actor

    -- Result
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument
