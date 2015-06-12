module Doc.API.V2.Calls.SignatoryCalls (
  docApiV2SigSetAuthentication
, docApiV2SigReject
, docApiV2SigCheck
, docApiV2SigSign
, docApiV2SigSendSmsPin
, docApiV2SigSetAttachment
) where

import Happstack.Server.Types

import API.Monad.V2
import Chargeable.Model
import Control.Exception.Lifted
import DB
import Doc.API.V2.CallsUtils
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
import Util.SignatoryLinkUtils

docApiV2SigSetAuthentication :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthentication _did _slid = $undefined -- TODO implement

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject _did _slid = $undefined -- TODO implement

docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck _did _slid = $undefined -- TODO implement

docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = api $ do
  (mh,_mu) <- getMagicHashAndUserForSignatoryAction did slid
  screenshots <- getScreenshots
  fields <- apiV2Parameter' (ApiV2ParameterJSON "fields" Obligatory unjsonSignatoryFieldsValues)
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    guardThatDocument (hasSigned . $fromJust . getSigLinkFor slid ) "Document can't be already signed"
    checkAuthenticationMethodAndValue slid
    authorization <- signatorylinkauthenticationmethod <$> $fromJust . getSigLinkFor slid <$> theDocument

    case authorization of
      StandardAuthentication -> do
        signDocument slid mh fields Nothing Nothing screenshots
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument

      SMSPinAuthentication -> do
        validPin <- getValidPin slid fields
        if (isJust validPin)
          then do
            signDocument slid mh fields Nothing validPin screenshots
            postDocumentPendingChange olddoc
            handleAfterSigning slid
            Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
          else throwIO . SomeKontraException $ documentActionForbidden

      ELegAuthentication -> dbQuery (GetESignature slid) >>= \case
        mesig@(Just _) -> do
          -- charge company of the author of the document for the signature
          dbUpdate $ ChargeCompanyForElegSignature did
          signDocument slid mh fields mesig Nothing screenshots
          postDocumentPendingChange olddoc
          handleAfterSigning slid
          Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
        Nothing -> throwIO . SomeKontraException $ documentActionForbidden
   )

docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin _did _slid = $undefined -- TODO implement

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment _did _slid = $undefined -- TODO implement
