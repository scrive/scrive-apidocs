module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigReject
, docApiV2SigForwardSigning
, docApiV2SigCheck
, docApiV2SigApprove
, docApiV2SigSign
, docApiV2SigIdentifyToViewWithSmsPin
, docApiV2SigSendSmsPinToSign
, docApiV2SigSendSmsPinToView
, docApiV2SigSetAttachment
, docApiV2SigSigningStatusCheck
, docApiV2SigSigningCancel
, docApiV2SetHighlightForPage
) where

import Control.Conditional (whenM)
import Control.Monad.Extra (whenJustM, whenMaybe)
import Data.Bifunctor
import Data.Unjson
import Happstack.Server.Types
import Log
import Text.JSON.Types (JSValue(..))
import qualified Data.Text as T
import qualified Text.JSON as J
import qualified Text.StringTemplates.Fields as F

import API.V2
import API.V2.Parameters
import DB
import Doc.Action
import Doc.API.V2.Calls.SignatoryCallsUtils
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.DocControl
import Doc.DocMails (sendPinCode)
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Flow (processEventThrow)
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Signing.Model
import Doc.SMSPin.Model
import EID.Authentication.Model
  ( EAuthentication(..), MergeDocumentEidAuthentication(..)
  )
import EID.EIDService.Provider.SEBankID (useEIDHubForSEBankIDSign)
import EID.Signature.Model
import EvidenceLog.Model
import File.Types (File(..))
import InputValidation (Result(..), asValidPhoneForSMS)
import Kontra
import Session.Model (getCurrentSession)
import Session.Types (Session(sesID))
import User.Lang
import UserGroup (ugwpSettings)
import UserGroup.Model (UserGroupGetWithParents(..))
import Util.Actor
import Util.HasSomeUserInfo (getMobile)
import Util.MonadUtils (guardJust, guardJustM)
import Util.SignatoryLinkUtils
import qualified Flow.Engine as Flow
import qualified Flow.Machinize as Flow
import qualified Flow.Model as Flow (selectInstanceIdByDocumentId)

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do

    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSigningPartyHasNeitherSignedNorApproved slid =<< theDocument

    -- Parameters
    rejectReason <- fmap T.strip <$> apiV2ParameterOptional (ApiV2ParameterText "reason")

    -- API call actions
    sl           <- guardGetSignatoryFromIdForDocument slid
    ctx          <- getContext
    actor        <- signatoryActor ctx sl

    switchLang . getLang =<< theDocument
    dbUpdate $ RejectDocument slid (isApprover sl) rejectReason actor
    postDocumentRejectedChange slid rejectReason =<< theDocument

    -- Result
    doc <- theDocument
    -- If document is part of a flow instance, send relevant event.
    whenJustM (Flow.selectInstanceIdByDocumentId did) processFlowEvent
    return $ Ok (unjsonDocument (documentAccessForSlid slid doc), doc)
  where
    processFlowEvent instanceId = processEventThrow $ Flow.EngineEvent
      { instanceId  = instanceId
      , userAction  = Flow.Rejection
      , signatoryId = slid
      , documentId  = did
      }


docApiV2SigForwardSigning :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigForwardSigning did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do

    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSigningPartyHasNeitherSignedNorApproved slid =<< theDocument
    guardThatDocumentHasntBeenForwadedTooManyTimes =<< theDocument

    -- Parameters
    messageParam :: Maybe Text <- apiV2ParameterOptional (ApiV2ParameterText "message")
    let forwardMessage = T.strip <$> messageParam

    (SignatoryTextFieldIDsWithNewTexts textFieldIDsWithNewText) <-
      apiV2ParameterObligatory
        (ApiV2ParameterJSON "fields" unjsonSignatoryTextFieldIDsWithNewTexts)

    -- API call actions
    originalSignatory <- guardGetSignatoryFromIdForDocument slid
    ctx               <- getContext
    actor             <- signatoryActor ctx originalSignatory

    switchLang . getLang =<< theDocument
    newslid <- dbUpdate
      $ ForwardSigning originalSignatory forwardMessage textFieldIDsWithNewText actor

    postDocumentForwardChange forwardMessage originalSignatory newslid =<< theDocument

    -- Result
    doc <- theDocument
    return $ Ok (unjsonDocument (documentAccessForSlid slid doc), doc)

docApiV2SigSigningStatusCheck
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSigningStatusCheck did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  mstatus <- dbQuery $ GetDocumentSigningStatus slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    sl <- guardGetSignatoryFromIdForDocument slid
    (inProgress, signed, mLastStatus) <- case (isSignatoryAndHasSigned sl, mstatus) of
      (True, _) -> return (False, True, Nothing)
      (False, Just (cancelled, mLastStatus)) ->
        return (not cancelled, False, mLastStatus)
      (False, Nothing) -> do
        logAttention_
          "docApiV2SigSigningStatusCheck, sig hasnt signed, but no status in db"
        internalError
    logInfo "Status Check" $ object
      ["in_progress" .= inProgress, "signed" .= signed, "status" .= mLastStatus]
    return . Ok $ JSObject
      (J.toJSObject
        [ ("in_progress", JSBool inProgress)
        , ("signed"     , JSBool signed)
        , ( "last_check_status"
          , case mLastStatus of
            Nothing -> JSNull
            Just t  -> JSString . J.toJSString $ T.unpack t
          )
        ]
      )

docApiV2SigSigningCancel :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSigningCancel did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid

  -- We can't allow cancelling signing on single document as it should be done on flow.
  guardNotInFlow did
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    -- Guards
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    dbUpdate $ CleanAllScheduledDocumentSigning slid
    return $ Ok ()


docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardSignatoryNeedsToIdentifyToView slid =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    guardDocumentStatus Pending =<< theDocument
    -- Parameters
    acceptedAuthorAttachments <- apiV2ParameterObligatory
      $ ApiV2ParameterJSON "accepted_author_attachments" unjsonDef
    notUploadedSignatoryAttachments <- apiV2ParameterDefault
      []
      (ApiV2ParameterJSON "not_uploaded_signatory_attachments" unjsonDef)
    guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAuthorAttachments
      =<< theDocument
    guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid
                                                        notUploadedSignatoryAttachments
      =<< theDocument
    checkAuthenticationToSignMethodAndValue slid
    fields <- apiV2ParameterObligatory
      (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValuesForSigning)
    let SignatoryFieldsValuesForSigning tmpFields = fields
        fieldsShortLog = map (second signatoryFieldTMPValueShortLog) tmpFields
    logInfo "Fields provided for signing" $ object ["fields" .= show fieldsShortLog]
    guardThatRadioButtonValuesAreValid slid fields =<< theDocument
    consentResponses <- apiV2ParameterDefault
      (SignatoryConsentResponsesForSigning [])
      (ApiV2ParameterJSON "consent_responses" unjsonSignatoryConsentResponsesForSigning)
    guardThatAllConsentQuestionsHaveResponse slid consentResponses =<< theDocument
    -- API call actions + extra conditional parameter
    sl <- guardGetSignatoryFromIdForDocument slid
    when (signatorylinkauthenticationtosignmethod sl == SMSPinAuthenticationToSign) $ do
      pin      <- apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
      validPin <- checkSignatoryPinToSign slid fields pin
      unless validPin $ do
        apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
    -- Return
    return $ Ok ()

docApiV2SigApprove :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigApprove did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  olddoc <- dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid)

  olddoc `withDocument` do

    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardSignatoryNeedsToIdentifyToView slid =<< theDocument
    guardSignatoryRoleIs SignatoryRoleApprover slid =<< theDocument
    guardApproverHasNotApproved slid =<< theDocument

    -- API call actions
    sl    <- guardGetSignatoryFromIdForDocument slid
    ctx   <- getContext
    actor <- signatoryActor ctx sl

    switchLang . getLang =<< theDocument
    dbUpdate $ ApproveDocument slid actor
    postDocumentPendingChange olddoc sl

    -- Result
    doc <- theDocument
    -- If document is part of a flow instance, send relevant event.
    whenJustM (Flow.selectInstanceIdByDocumentId did) processFlowEvent
    return $ Ok (unjsonDocument (documentAccessForSlid slid doc), doc)
  where
    processFlowEvent instanceId = processEventThrow $ Flow.EngineEvent
      { instanceId  = instanceId
      , userAction  = Flow.Approval
      , signatoryId = slid
      , documentId  = did
      }

-- | In the case of SMS or 'standard' (i.e. none) authentication, sign the
-- document for the given signatory right away; otherwise schedule a signing
-- job. In the latter case, the frontend initiates the actual eid transaction,
-- and cron takes care of the signing.
docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = logDocumentAndSignatory did slid . api $ do
  ctx <- getContext
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  -- We store old document, as it is needed by postDocumentXXX calls
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkID did slid

  olddoc `withDocument` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardSignatoryNeedsToIdentifyToView slid =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    guardDocumentStatus Pending =<< theDocument

    -- Parameters
    checkAuthenticationToSignMethodAndValue slid
    screenshots         <- getScreenshots
    acceptedAttachments <- apiV2ParameterObligatory
      (ApiV2ParameterJSON "accepted_author_attachments" unjsonDef)
    notUploadedSignatoryAttachments <- apiV2ParameterDefault
      []
      (ApiV2ParameterJSON "not_uploaded_signatory_attachments" unjsonDef)
    fields <- apiV2ParameterObligatory
      (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValuesForSigning)
    let SignatoryFieldsValuesForSigning tmpFields = fields
        fieldsShortLog = map (second signatoryFieldTMPValueShortLog) tmpFields
    logInfo "Fields provided for signing" $ object ["fields" .= show fieldsShortLog]
    guardThatRadioButtonValuesAreValid slid fields =<< theDocument
    guardThatSignaturesAreFilled slid fields =<< theDocument
    consentResponses <- apiV2ParameterDefault
      (SignatoryConsentResponsesForSigning [])
      (ApiV2ParameterJSON "consent_responses" unjsonSignatoryConsentResponsesForSigning)

    -- Additional guards
    guardThatAllConsentQuestionsHaveResponse slid consentResponses =<< theDocument
    guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments =<< theDocument
    guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid
                                                        notUploadedSignatoryAttachments
      =<< theDocument

    -- API call actions + extra conditional parameter
    sl   <- guardGetSignatoryFromIdForDocument slid
    mpin <-
      whenMaybe (signatorylinkauthenticationtosignmethod sl == SMSPinAuthenticationToSign)
        $ do
            pin      <- apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
            validPin <- checkSignatoryPinToSign slid fields pin
            if not validPin then apiError documentActionForbidden else return pin

    seBankIDSignProvider <- do
      authorugid <- guardJust $ documentauthorugid olddoc
      ugwp       <- guardJustM . dbQuery $ UserGroupGetWithParents authorugid
      return $ if useEIDHubForSEBankIDSign ctx (ugwpSettings ugwp)
        then EIDServiceSEBankID
        else CgiGrpBankID

    let
      useEIDHubForNOBankIDSign =
        fromMaybe False $ ctx ^? #eidServiceConf % _Just % #eidUseForNOSign
      mprovider = case signatorylinkauthenticationtosignmethod sl of
        StandardAuthenticationToSign -> Nothing
        SMSPinAuthenticationToSign   -> Nothing
        SEBankIDAuthenticationToSign -> Just seBankIDSignProvider
        NOBankIDAuthenticationToSign | useEIDHubForNOBankIDSign -> Just EIDServiceNOBankID
                                     | otherwise                -> Just NetsNOBankID
        DKNemIDAuthenticationToSign             -> Just NetsDKNemID
        IDINAuthenticationToSign                -> Just EIDServiceIDIN
        FITupasAuthenticationToSign             -> Just EIDServiceTupas
        OnfidoDocumentCheckAuthenticationToSign -> Just EIDServiceOnfido
        OnfidoDocumentAndPhotoCheckAuthenticationToSign -> Just EIDServiceOnfido
        VerimiQesAuthenticationToSign           -> Just EIDServiceVerimi

    case mprovider of
      Nothing -> do
        signDocument slid
                     fields
                     acceptedAttachments
                     notUploadedSignatoryAttachments
                     Nothing
                     mpin
                     screenshots
                     consentResponses
        updatedSigLink <- guardGetSignatoryFromIdForDocument slid
        postDocumentPendingChange olddoc updatedSigLink
        handleAfterSigning slid
        -- If document is part of a flow instance, send relevant event.
        whenJustM (Flow.selectInstanceIdByDocumentId did) processFlowEvent
       -- Return
      Just provider -> do
        doclang <- getLang <$> theDocument
        dbUpdate $ CleanAllScheduledDocumentSigning slid
        dbUpdate $ ScheduleDocumentSigning slid
                                           (ctx ^. #brandedDomain % #id)
                                           (ctx ^. #time)
                                           (ctx ^. #ipAddr)
                                           (ctx ^. #clientTime)
                                           (ctx ^. #clientName)
                                           doclang
                                           fields
                                           acceptedAttachments
                                           screenshots
                                           notUploadedSignatoryAttachments
                                           provider
                                           consentResponses
    doc <- theDocument
    return $ Ok (unjsonDocument (documentAccessForSlid slid doc), doc)
  where
    processFlowEvent instanceId = processEventThrow $ Flow.EngineEvent
      { instanceId  = instanceId
      , userAction  = Flow.Signature
      , signatoryId = slid
      , documentId  = did
      }

docApiV2SigSendSmsPinToSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPinToSign did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    sl <- guardGetSignatoryFromIdForDocument slid
    when (signatorylinkauthenticationtosignmethod sl /= SMSPinAuthenticationToSign) $ do
      apiError
        $ signatoryStateError "Signatory authentication method to sign is not SMS PIN"
    -- Parameters
    let mobileEditableBySignatory =
          Just True
            == (   fieldEditableBySignatory
               =<< getFieldByIdentity MobileFI (signatoryfields sl)
               )
    let slidMobile = getMobile sl
    mobile <- if not (T.null slidMobile) && not mobileEditableBySignatory
      then case asValidPhoneForSMS slidMobile of
        Good v -> return v
        _ ->
          apiError $ serverError "Mobile number for signatory set by author is not valid"
      else apiV2ParameterObligatory
        (ApiV2ParameterTextWithValidation "mobile" asValidPhoneForSMS)
    -- API call actions
    pin <- dbQuery $ GetSignatoryPin SMSPinToSign slid mobile
    sendPinCode sl mobile pin
    -- Return
    return $ Accepted ()

guardAuthenticateToViewWithPin
  :: (Kontrakcja m, DocumentMonad m)
  => DocumentID
  -> SignatoryLinkID
  -> m (AuthenticationKind, SignatoryLink)
guardAuthenticateToViewWithPin did slid = do
  guardThatObjectVersionMatchesIfProvided did
  signatoryAuthMethod <- theDocument >>= \doc -> case documentstatus doc of
    Pending -> do
      return signatorylinkauthenticationtoviewmethod
    Closed -> return signatorylinkauthenticationtoviewarchivedmethod
    _ -> apiError $ documentStateError "The document status should be Pending or Closed"
  sl <- guardGetSignatoryFromIdForDocument slid
  when (signatoryAuthMethod sl /= SMSPinAuthenticationToView) $ do
    apiError
      $ signatoryStateError "Signatory authentication method to view is not SMS PIN"
  (, sl) . mkAuthKind <$> theDocument

docApiV2SigSendSmsPinToView :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPinToView did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    (authKind, sl) <- guardAuthenticateToViewWithPin did slid
    mobile         <- case asValidPhoneForSMS (getMobile sl) of
      Good v -> return v
      _ ->
        apiError $ serverError "Mobile number for signatory set by author is not valid"
    -- API call actions
    pin <- dbQuery $ GetSignatoryPin (authKindToPinType authKind) slid mobile
    sendPinCode sl mobile pin
    -- Return
    return $ Accepted ()

docApiV2SigIdentifyToViewWithSmsPin
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigIdentifyToViewWithSmsPin did slid =
  logDocumentAndSignatory did slid . api $ do
  -- Permissions
    guardAccessToDocumentWithSignatory did slid
    ctx <- getContext
    dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
      (authKind, sl) <- guardAuthenticateToViewWithPin did slid
      mobile         <- case asValidPhoneForSMS (getMobile sl) of
        Good v -> return v
        _ ->
          apiError $ serverError "Mobile number for signatory set by author is not valid"
      pin      <- apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
      validPin <- checkSignatoryPinToView (authKindToPinType authKind) slid pin
      unless validPin $ do
        apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
      sess <- getCurrentSession
      dbUpdate
        . MergeDocumentEidAuthentication authKind (sesID sess) slid
        $ SMSPinAuthentication_ mobile
      let eventFields = do
            F.value "signatory_mobile" mobile
            F.value "provider_sms_pin" True
      sActor <- signatoryActor ctx sl
      -- Record evidence only for auth-to-view (i.e. if the document is not
      -- closed).
      whenM ((== AuthenticationToView) . mkAuthKind <$> theDocument) $ do
        void . dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
          AuthenticatedToViewEvidence
          eventFields
          (Just sl)
          Nothing
          sActor
      return $ Accepted ()

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    name        <- apiV2ParameterObligatory (ApiV2ParameterText "name")
    mAttachment <- apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage "attachment")
    doc         <- theDocument
    let mSigAttachment = getSignatoryAttachment slid name doc
    sigAttachment <- case mSigAttachment of
      Nothing -> apiError $ requestParameterInvalid
        "name"
        "There is no attachment with that name for the signatory"
      Just sa -> return sa
    -- API call actions
    sl  <- guardGetSignatoryFromIdForDocument slid
    ctx <- getContext
    case mAttachment of
      Nothing ->
        dbUpdate . DeleteSigAttachment slid sigAttachment =<< signatoryActor ctx sl
      Just file ->
        (   dbUpdate
          .   SaveSigAttachment slid sigAttachment (fileid file)
          =<< signatoryActor ctx sl
          )
          `catchDBExtraException` (\(DBBaseLineConditionIsFalse _) ->
                                    apiError
                                      $ signatoryStateError
                                          "An attachment of this name for this signatory and document is already set, remove it first."
                                  )
    -- Return
    updatedDoc <- theDocument
    return $ Ok (unjsonDocument (documentAccessForSlid slid updatedDoc), updatedDoc)

docApiV2SetHighlightForPage :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SetHighlightForPage did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  guardAccessToDocumentWithSignatory did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkID did slid) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    page   <- apiV2ParameterObligatory (ApiV2ParameterInt "page")
    mimage <- apiV2ParameterOptional (ApiV2ParameterBase64PNGImage "image")
    -- API call actions
    ctx    <- getContext
    sl     <- guardGetSignatoryFromIdForDocument slid
    actor  <- signatoryActor ctx sl
    case mimage of
      Just image -> dbUpdate
        $ SetHighlightingForPageAndSignatory sl (fromIntegral page) (fileid image) actor
      Nothing ->
        dbUpdate $ ClearHighlightingForPageAndSignatory sl (fromIntegral page) actor
    -- Result
    doc <- theDocument
    return $ Ok (unjsonDocument (documentAccessForSlid slid doc), doc)
