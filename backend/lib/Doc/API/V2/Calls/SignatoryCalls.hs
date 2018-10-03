module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigReject
, docApiV2SigCheck
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
import Data.Unjson
import Happstack.Server.Types
import Text.JSON.Types (JSValue(..))
import qualified Data.Text as T
import qualified Text.JSON as J
import qualified Text.StringTemplates.Fields as F

import API.V2
import API.V2.Parameters
import BrandedDomain.BrandedDomain
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
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Signing.Model
import Doc.SMSPin.Model
import EID.Authentication.Model (MergeSMSPinAuthentication(..))
import EID.Signature.Model
import EvidenceLog.Model
import File.File (File(..))
import InputValidation (Result(..), asValidPhoneForSMS)
import Kontra
import Session.Data (Session(sesID))
import Session.Model (getCurrentSession)
import User.Lang
import Util.Actor
import Util.HasSomeUserInfo (getMobile)
import Util.SignatoryLinkUtils

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    rejectReason <- (fmap $ T.unpack . T.strip) <$> apiV2ParameterOptional (ApiV2ParameterText "reason")
    -- API call actions
    ctx <- getContext
    sl <- guardGetSignatoryFromIdForDocument slid
    actor <- signatoryActor ctx sl
    switchLang . getLang =<< theDocument
    dbUpdate $ RejectDocument slid rejectReason actor
    postDocumentRejectedChange slid rejectReason =<< theDocument
    -- Result
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc


docApiV2SigSigningStatusCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSigningStatusCheck did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    sl <- guardGetSignatoryFromIdForDocument slid
    isDocumentSigningInProgress <- dbQuery $ IsDocumentSigningInProgress slid
    lastCheckStatus <- dbQuery $ GetDocumentSigningLastCheckStatus slid
    return $ Ok $ JSObject (J.toJSObject $ [
        ("in_progress", JSBool isDocumentSigningInProgress)
      , ("signed", JSBool $ hasSigned sl)
      , ("last_check_status", case lastCheckStatus of
          Nothing -> JSNull
          Just t -> JSString $ J.toJSString $ T.unpack t
        )
      ])

docApiV2SigSigningCancel :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSigningCancel did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    dbUpdate $ CleanAllScheduledDocumentSigning slid
    return $ Ok $ ()


docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardSignatoryNeedsToIdentifyToView slid =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    guardDocumentStatus Pending =<< theDocument
    -- Parameters
    acceptedAuthorAttachments <- apiV2ParameterObligatory $ ApiV2ParameterJSON "accepted_author_attachments" unjsonDef
    notUploadedSignatoryAttachments <- apiV2ParameterDefault [] (ApiV2ParameterJSON "not_uploaded_signatory_attachments" unjsonDef)
    guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAuthorAttachments =<< theDocument
    guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid notUploadedSignatoryAttachments =<< theDocument
    checkAuthenticationToSignMethodAndValue slid
    fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValuesForSigning)
    guardThatRadioButtonValuesAreValid slid fields =<< theDocument
    consentResponses <- apiV2ParameterDefault
      (SignatoryConsentResponsesForSigning [])
      (ApiV2ParameterJSON "consent_responses" unjsonSignatoryConsentResponsesForSigning)
    guardThatAllConsentQuestionsHaveResponse slid consentResponses =<< theDocument
    -- API call actions + extra conditional parameter
    sl <- guardGetSignatoryFromIdForDocument slid
    case signatorylinkauthenticationtosignmethod sl of
      StandardAuthenticationToSign -> return ()
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPinToSign slid fields pin
        if not validPin
           then apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
           else return ()
      SEBankIDAuthenticationToSign -> return ()
      NOBankIDAuthenticationToSign -> return ()
      DKNemIDAuthenticationToSign  -> return ()
    -- Return
    return $ Ok ()

docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardSignatoryNeedsToIdentifyToView slid =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    guardDocumentStatus Pending =<< theDocument
    -- Parameters
    checkAuthenticationToSignMethodAndValue slid
    screenshots <- getScreenshots
    acceptedAttachments <- apiV2ParameterObligatory (ApiV2ParameterJSON "accepted_author_attachments" unjsonDef)
    notUploadedSignatoryAttachments <- apiV2ParameterDefault [] (ApiV2ParameterJSON "not_uploaded_signatory_attachments" unjsonDef)
    fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValuesForSigning)
    guardThatRadioButtonValuesAreValid slid fields =<< theDocument
    consentResponses <- apiV2ParameterDefault
      (SignatoryConsentResponsesForSigning [])
      (ApiV2ParameterJSON "consent_responses" unjsonSignatoryConsentResponsesForSigning)
    guardThatAllConsentQuestionsHaveResponse slid consentResponses =<< theDocument
    -- API call actions + extra conditional parameter
    guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments =<< theDocument
    guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid notUploadedSignatoryAttachments =<< theDocument
    sl <- guardGetSignatoryFromIdForDocument slid
    (mprovider, mpin) <- case signatorylinkauthenticationtosignmethod sl of
      StandardAuthenticationToSign -> return (Nothing, Nothing)
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPinToSign slid fields pin
        if not validPin
          then apiError documentActionForbidden
          else return (Nothing, Just pin)
      SEBankIDAuthenticationToSign -> return (Just CgiGrpBankID, Nothing)
      NOBankIDAuthenticationToSign -> return (Just NetsNOBankID, Nothing)
      DKNemIDAuthenticationToSign  -> return (Just NetsDKNemID, Nothing)
    case mprovider of
       Nothing -> do
        signDocument slid mh fields acceptedAttachments notUploadedSignatoryAttachments Nothing mpin screenshots consentResponses
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        -- Return
       Just provider -> do
        ctx <- getContext
        doclang <- getLang <$> theDocument
        dbUpdate $ CleanAllScheduledDocumentSigning slid
        dbUpdate $ ScheduleDocumentSigning
          slid
          (get (bdid . ctxbrandeddomain) ctx)
          (get ctxtime ctx)
          (get ctxipnumber ctx)
          (get ctxclienttime ctx)
          (get ctxclientname ctx)
          doclang
          fields
          acceptedAttachments
          screenshots
          notUploadedSignatoryAttachments
          provider
          consentResponses
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc

docApiV2SigSendSmsPinToSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPinToSign did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    sl <- guardGetSignatoryFromIdForDocument slid
    when (signatorylinkauthenticationtosignmethod sl /= SMSPinAuthenticationToSign) $ do
      apiError $ signatoryStateError "Signatory authentication method to sign is not SMS PIN"
    -- Parameters
    let mobileEditableBySignatory = Just True == join (fieldEditableBySignatory <$> getFieldByIdentity MobileFI (signatoryfields sl))
    let slidMobile = getMobile sl
    mobile <- if (not (null slidMobile) && not mobileEditableBySignatory)
                then case asValidPhoneForSMS slidMobile of
                          Good v -> return v
                          _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
                else T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterTextWithValidation "mobile" asValidPhoneForSMS)
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
      guardSignatoryHasNotSigned slid doc
      return signatorylinkauthenticationtoviewmethod
    Closed  -> return signatorylinkauthenticationtoviewarchivedmethod
    _       -> apiError $ documentStateError
                            "The document status should be Pending or Closed"
  sl <- guardGetSignatoryFromIdForDocument slid
  when (signatoryAuthMethod sl /= SMSPinAuthenticationToView) $ do
    apiError $ signatoryStateError "Signatory authentication method to view is not SMS PIN"
  (, sl) . mkAuthKind <$> theDocument

docApiV2SigSendSmsPinToView :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPinToView did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    (authKind, sl) <- guardAuthenticateToViewWithPin did slid
    mobile <- case asValidPhoneForSMS (getMobile sl) of
                Good v -> return v
                _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
    -- API call actions
    pin <- dbQuery $ GetSignatoryPin (authKindToPinType authKind) slid mobile
    sendPinCode sl mobile pin
    -- Return
    return $ Accepted ()

docApiV2SigIdentifyToViewWithSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigIdentifyToViewWithSmsPin did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  ctx <- getContext
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    (authKind, sl) <- guardAuthenticateToViewWithPin did slid
    mobile <- case asValidPhoneForSMS (getMobile sl) of
                Good v -> return v
                _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
    pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
    validPin <- checkSignatoryPinToView (authKindToPinType authKind) slid pin
    when (not validPin) $ do
      apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
    sess <- getCurrentSession
    dbUpdate $ MergeSMSPinAuthentication authKind (sesID sess) slid (T.pack mobile)
    let eventFields = do
          F.value "signatory_mobile" mobile
          F.value "provider_sms_pin" True
    sActor <- signatoryActor ctx sl
    -- Record evidence only for auth-to-view (i.e. if the document is not
    -- closed).
    whenM ((== AuthenticationToView) . mkAuthKind <$> theDocument) $ do
      void $ dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence (eventFields) (Just sl) Nothing sActor
    return $ Accepted ()

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    name <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "name")
    mAttachment <- apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage "attachment")
    doc <- theDocument
    let mSigAttachment = getSignatoryAttachment slid name doc
    sigAttachment <- case mSigAttachment of
      Nothing -> apiError $ requestParameterInvalid "name" "There is no attachment with that name for the signatory"
      Just sa -> return sa
    -- API call actions
    sl <- guardGetSignatoryFromIdForDocument slid
    ctx <- getContext
    case mAttachment of
      Nothing -> dbUpdate . DeleteSigAttachment slid sigAttachment =<< signatoryActor ctx sl
      Just file ->
        (dbUpdate . SaveSigAttachment slid sigAttachment (fileid file) =<< signatoryActor ctx sl)
        `catchDBExtraException`
        (\(DBBaseLineConditionIsFalse _) -> apiError $ signatoryStateError
          "An attachment of this name for this signatory and document is already set, remove it first.")
    -- Return
    updatedDoc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid updatedDoc),d)) updatedDoc

docApiV2SetHighlightForPage :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SetHighlightForPage did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    -- Parameters
    page <- apiV2ParameterObligatory (ApiV2ParameterInt "page")
    mimage <- apiV2ParameterOptional (ApiV2ParameterBase64PNGImage "image")
    -- API call actions
    ctx <- getContext
    sl <- guardGetSignatoryFromIdForDocument slid
    actor <- signatoryActor ctx sl
    case mimage of
       Just image -> dbUpdate $ SetHighlightingForPageAndSignatory sl (fromIntegral page) (fileid image) actor
       Nothing    -> dbUpdate $ ClearHighlightingForPageAndSignatory sl (fromIntegral page) actor
    -- Result
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc
