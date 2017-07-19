module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigReject
, docApiV2SigCheck
, docApiV2SigSign
, docApiV2SigSendSmsPin
, docApiV2SigSetAttachment
, docApiV2SigSigningStatusCheck
, docApiV2SigSigningCancel
, docApiV2SetHighlightForPage
) where

import Data.Unjson
import Happstack.Server.Types
import Text.JSON.Types (JSValue(..))
import qualified Data.Text as T
import qualified Text.JSON as J

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
import Doc.DocControl
import Doc.DocMails (sendPinCode)
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils (getSignatoryAttachment)
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Signing.Model
import Doc.SMSPin.Model (GetSignatoryPin(..))
import File.File (File(..))
import InputValidation (Result(..), asValidPhoneForSMS)
import Kontra
import KontraPrelude
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
    Just sl <- getSigLinkFor slid <$> theDocument
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
    Just sl <- getSigLinkFor slid <$> theDocument
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
    -- API call actions + extra conditional parameter
    authorization <- signatorylinkauthenticationtosignmethod <$> fromJust . getSigLinkFor slid <$> theDocument
    case authorization of
      StandardAuthenticationToSign -> return ()
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if not validPin
           then apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
           else return ()
      SEBankIDAuthenticationToSign -> return ()
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
    -- API call actions + extra conditional parameter
    guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments =<< theDocument
    guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid notUploadedSignatoryAttachments =<< theDocument
    authorization <- signatorylinkauthenticationtosignmethod <$> fromJust . getSigLinkFor slid <$> theDocument
    (signNow, mpin) <- case authorization of
      StandardAuthenticationToSign -> return (True, Nothing)
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if not validPin
          then apiError documentActionForbidden
          else return (True, Just pin)
      SEBankIDAuthenticationToSign -> return (False, Nothing)
    if (signNow)
       then do
        signDocument slid mh fields acceptedAttachments notUploadedSignatoryAttachments Nothing mpin screenshots
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        -- Return
       else do
        ctx <- getContext
        doclang <- getLang <$> theDocument
        dbUpdate $ CleanAllScheduledDocumentSigning slid
        dbUpdate $ ScheduleDocumentSigning
          slid
          (bdid $ ctxbrandeddomain ctx)
          (ctxtime ctx)
          (ctxipnumber ctx)
          (ctxclienttime ctx)
          (ctxclientname ctx)
          doclang
          fields
          acceptedAttachments
          screenshots
          notUploadedSignatoryAttachments
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc

docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending =<< theDocument
    guardSignatoryHasNotSigned slid =<< theDocument
    sl <- fromJust . getSigLinkFor slid <$> theDocument
    when (signatorylinkauthenticationtosignmethod sl /= SMSPinAuthenticationToSign) $ do
      apiError $ signatoryStateError "Signatory authentication method to sign is not SMS PIN"
    -- Parameters
    let mobileEditableBySignatory = Just True == join (fieldEditableBySignatory <$> getFieldByIdentity MobileFI (signatoryfields sl))
    let slidMobile = getMobile sl
    mobile <- if (not (null slidMobile) && not mobileEditableBySignatory)
                then case asValidPhoneForSMS slidMobile of
                          Good v -> return v
                          _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
                else do
                    mobileParam <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "mobile")
                    case asValidPhoneForSMS mobileParam of
                         Good v -> return v
                         _ -> apiError $ requestParameterInvalid "mobile" "Not a valid mobile number"
    -- API call actions
    pin <- dbQuery $ GetSignatoryPin slid mobile
    sendPinCode sl mobile pin
    -- Return
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
    sl <- fromJust . getSigLinkFor slid <$> theDocument
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
    Just sl <- getSigLinkFor slid <$> theDocument
    actor <- signatoryActor ctx sl
    case mimage of
       Just image -> dbUpdate $ SetHighlightingForPageAndSignatory sl (fromIntegral page) (fileid image) actor
       Nothing    -> dbUpdate $ ClearHighlightingForPageAndSignatory sl (fromIntegral page) actor
    -- Result
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc
