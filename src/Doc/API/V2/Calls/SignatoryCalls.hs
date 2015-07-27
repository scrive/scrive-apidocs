module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigReject
, docApiV2SigCheck
, docApiV2SigSign
, docApiV2SigSendSmsPin
, docApiV2SigSetAttachment
) where

import Data.Text (strip, unpack)
import Happstack.Server.Types

import API.V2
import DB
import Doc.API.V2.Calls.SignatoryCallsUtils
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.API.V2.JSONDocument
import Doc.API.V2.JSONFields
import Doc.API.V2.Parameters
import Doc.Action
import Doc.DocControl
import Doc.DocMails (sendPinCode)
import Doc.DocStateData
import Doc.DocUtils (getSignatoryAttachment)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SMSPin.Model (GetSignatoryPin(..))
import Doc.SignatoryLinkID
import EID.Signature.Model
import File.File (File(..))
import InputValidation (Result(..), asValidPhoneForSMS)
import Kontra
import KontraPrelude
import User.Lang
import Util.Actor
import Util.SignatoryLinkUtils

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  rejectReason' <- apiV2ParameterOptional (ApiV2ParameterText "reason")
  let rejectReason = fmap (unpack . strip) rejectReason'
  guardThatObjectVersionMatchesIfProvided did
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    guardDocumentStatus Pending
    ctx <- getContext
    Just sl <- getSigLinkFor slid <$> theDocument
    actor <- signatoryActor ctx sl
    switchLang . getLang =<< theDocument
    dbUpdate $ RejectDocument slid rejectReason actor
    postDocumentRejectedChange slid rejectReason =<< theDocument
    Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument

docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    guardSignatoryNeedsToIdentifyToView slid
    -- Parameters, API call actions, and return
    fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValues)
    checkAuthenticationToSignMethodAndValue slid
    authorization <- signatorylinkauthenticationtosignmethod <$> $fromJust . getSigLinkFor slid <$> theDocument
    case authorization of
      StandardAuthenticationToSign -> return $ Ok ()
      SMSPinAuthenticationToSign -> do
        pin <- fmap unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if validPin then return $ Ok ()
                    else apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
      SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
        Just _ -> return $ Ok ()
        Nothing -> apiError $ signatoryStateError "Swedish BankID authentication needed before signing."


docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  screenshots <- getScreenshots
  fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValues)
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    guardSignatoryNeedsToIdentifyToView slid
    checkAuthenticationToSignMethodAndValue slid
    authorization <- signatorylinkauthenticationtosignmethod <$> $fromJust . getSigLinkFor slid <$> theDocument

    case authorization of
      StandardAuthenticationToSign -> do
        signDocument slid mh fields Nothing Nothing screenshots
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument

      SMSPinAuthenticationToSign -> do
        pin <- fmap unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if validPin
          then do
            signDocument slid mh fields Nothing (Just pin) screenshots
            postDocumentPendingChange olddoc
            handleAfterSigning slid
            Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
          else apiError documentActionForbidden

      SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
        mesig@(Just _) -> do
          signDocument slid mh fields mesig Nothing screenshots
          postDocumentPendingChange olddoc
          handleAfterSigning slid
          Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
        Nothing -> apiError documentActionForbidden
   )

docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    sl <- $fromJust . getSigLinkFor slid <$> theDocument
    when (signatorylinkauthenticationtosignmethod sl /= SMSPinAuthenticationToSign) $ do
      apiError $ signatoryStateError "Signatory authentication method to sign is not SMS PIN"
    phone <- liftM unpack $ apiV2ParameterObligatory (ApiV2ParameterText "phone")
    case asValidPhoneForSMS phone of
      Good validPhone -> do
        pin <- dbQuery $ GetSignatoryPin slid validPhone
        sendPinCode sl validPhone pin
        return $ Accepted ()
      _ -> apiError $ requestParameterInvalid "phone" "Not a valid phone number"

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    -- Parameters
    name <- liftM unpack $ apiV2ParameterObligatory (ApiV2ParameterText "name")
    mAttachment <- apiV2ParameterOptional (ApiV2ParameterFilePDFOrImage "attachment")
    doc <- theDocument
    let mSigAttachment = getSignatoryAttachment slid name doc
    sigAttachment <- case mSigAttachment of
      Nothing -> apiError $ requestParameterInvalid "name" "There is no attachment with that name for the signatory"
      Just sa -> return sa
    -- API call actions
    sl <- $fromJust . getSigLinkFor slid <$> theDocument
    ctx <- getContext
    case mAttachment of
      Nothing -> dbUpdate . DeleteSigAttachment slid sigAttachment =<< signatoryActor ctx sl
      Just file ->
        (dbUpdate . SaveSigAttachment slid sigAttachment (fileid file) =<< signatoryActor ctx sl)
        `catchKontra`
        (\(DBBaseLineConditionIsFalse _) -> apiError $ signatoryStateError
          "An attachment of this name for this signatory and document is already set, remove it first.")
    -- Return
    Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
