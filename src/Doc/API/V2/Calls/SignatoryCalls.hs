module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigReject
, docApiV2SigCheck
, docApiV2SigSign
, docApiV2SigSendSmsPin
, docApiV2SigSetAttachment
) where

import Happstack.Server.Types
import qualified Data.Text as T

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
import Doc.Logging
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
import Util.HasSomeUserInfo (getMobile)
import Util.SignatoryLinkUtils

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
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

docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    guardSignatoryNeedsToIdentifyToView slid
    -- Parameters
    checkAuthenticationToSignMethodAndValue slid
    fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValues)
    -- API call actions + extra conditional parameter
    authorization <- signatorylinkauthenticationtosignmethod <$> $fromJust . getSigLinkFor slid <$> theDocument
    case authorization of
      StandardAuthenticationToSign -> return ()
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if not validPin
           then apiError $ requestParameterInvalid "sms_pin" "invalid SMS PIN"
           else return ()
      SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
        Nothing -> apiError $ signatoryStateError "Swedish BankID authentication needed before signing."
        Just _ -> return ()
    -- Return
    return $ Ok ()


docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    guardSignatoryNeedsToIdentifyToView slid
    -- Parameters
    checkAuthenticationToSignMethodAndValue slid
    screenshots <- getScreenshots
    fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValues)
    -- API call actions + extra conditional parameter
    authorization <- signatorylinkauthenticationtosignmethod <$> $fromJust . getSigLinkFor slid <$> theDocument
    (mesig, mpin) <- case authorization of
      StandardAuthenticationToSign -> return (Nothing, Nothing)
      SMSPinAuthenticationToSign -> do
        pin <- fmap T.unpack $ apiV2ParameterObligatory (ApiV2ParameterText "sms_pin")
        validPin <- checkSignatoryPin slid fields pin
        if not validPin
          then apiError documentActionForbidden
          else return (Nothing, Just pin)
      SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
        Nothing -> apiError $ signatoryStateError "Swedish BankID authentication needed before signing."
        Just esig -> return (Just esig, Nothing)
    signDocument slid mh fields mesig mpin screenshots
    postDocumentPendingChange olddoc
    handleAfterSigning slid
    -- Return
    doc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid doc),d)) doc
   )

docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    sl <- $fromJust . getSigLinkFor slid <$> theDocument
    when (signatorylinkauthenticationtosignmethod sl /= SMSPinAuthenticationToSign) $ do
      apiError $ signatoryStateError "Signatory authentication method to sign is not SMS PIN"
    -- Parameters
    let slidPhone = getMobile sl
    phone <- if not $ null slidPhone
                then case asValidPhoneForSMS slidPhone of
                          Good v -> return v
                          _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
                else do
                    phoneParam <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "phone")
                    case asValidPhoneForSMS phoneParam of
                         Good v -> return v
                         _ -> apiError $ requestParameterInvalid "phone" "Not a valid phone number"
    -- API call actions
    pin <- dbQuery $ GetSignatoryPin slid phone
    sendPinCode sl phone pin
    -- Return
    return $ Accepted ()

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment did slid = logDocumentAndSignatory did slid . api $ do
  -- Permissions
  mh <- getMagicHashForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    -- Guards
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardSignatoryHasNotSigned slid
    -- Parameters
    name <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "name")
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
    updatedDoc <- theDocument
    return $ Ok $ (\d -> (unjsonDocument (documentAccessForSlid slid updatedDoc),d)) updatedDoc
