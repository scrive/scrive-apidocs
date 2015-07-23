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
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SignatoryLinkID
import EID.Signature.Model
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
docApiV2SigCheck _did _slid = $undefined -- TODO implement

docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = api $ do
  mh <- getMagicHashForSignatoryAction did slid
  screenshots <- getScreenshots
  fields <- apiV2ParameterObligatory (ApiV2ParameterJSON "fields" unjsonSignatoryFieldsValues)
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardThatDocument (hasSigned . $fromJust . getSigLinkFor slid ) "Document can't be already signed"
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
        pin <- fmap unpack $ apiV2ParameterObligatory (ApiV2ParameterText "pin")
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
docApiV2SigSendSmsPin _did _slid = $undefined -- TODO implement

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment _did _slid = $undefined -- TODO implement
