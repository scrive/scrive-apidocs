module Doc.API.V2.Calls.SignatoryCallsUtils (
  checkAuthenticationToSignMethodAndValue
, getScreenshots
, signDocument
, checkSignatoryPinToSign
, checkSignatoryPinToView
, fieldsToFieldsWithFiles
) where

import Control.Monad.Catch
import Control.Monad.Time
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import API.V2
import API.V2.Parameters
import DB
import Doc.API.V2.Guards (guardGetSignatoryFromIdForDocument)
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots (SignatoryScreenshots, emptySignatoryScreenshots, resolveReferenceScreenshotNames)
import Doc.SMSPin.Model
import EID.Signature.Model
import File.Model
import Kontra
import MagicHash (MagicHash)
import User.Model
import Util.Actor
import Util.HasSomeUserInfo

{- | Check if provided authorization values for sign call patch -}
checkAuthenticationToSignMethodAndValue :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
checkAuthenticationToSignMethodAndValue slid = do
  mAuthType <- apiV2ParameterOptional (ApiV2ParameterTextUnjson "authentication_type" unjsonAuthenticationToSignMethod)
  case mAuthType of
    Nothing -> return ()
    Just authMethod -> do
      siglink <- guardGetSignatoryFromIdForDocument slid
      let authOK = authMethod == signatorylinkauthenticationtosignmethod siglink
      when (not authOK) $
        apiError $ requestParameterInvalid "authentication_type" "does not match with document"
      case authMethod of
        StandardAuthenticationToSign -> return ()
        SEBankIDAuthenticationToSign -> checkParamSSNMatchesSigLink siglink
        NOBankIDAuthenticationToSign -> return ()
        DKNemIDAuthenticationToSign  -> checkParamSSNMatchesSigLink siglink
        SMSPinAuthenticationToSign   -> checkParamMobileMatchesSigLink siglink
  where
    checkParamMobileMatchesSigLink siglink = do
      authValue <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
      let mobileEditableBySignatory = Just True == join (fieldEditableBySignatory <$> getFieldByIdentity MobileFI (signatoryfields siglink))
      if (authValue == getMobile siglink || null (getMobile siglink) || mobileEditableBySignatory)
        then return ()
        else apiError $
          requestParameterInvalid "authentication_value" "value for mobile number does not match"
    checkParamSSNMatchesSigLink siglink = do
      authValue <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
      if (authValue == getPersonalNumber siglink || null (getPersonalNumber siglink))
        then return ()
        else apiError $
          requestParameterInvalid "authentication_value" "value for personal number does not match"

getScreenshots :: (Kontrakcja m) => m SignatoryScreenshots
getScreenshots = do
  screenshots <- apiV2ParameterDefault emptySignatoryScreenshots (ApiV2ParameterJSON "screenshots" unjsonSignatoryScreenshots)
  resolvedScreenshots <- resolveReferenceScreenshotNames screenshots
  case resolvedScreenshots of
    Nothing -> apiError $ requestParameterInvalid "screenshots" "Could not resolve reference screenshot"
    Just res -> return res



signDocument :: (Kontrakcja m, DocumentMonad m) =>
  SignatoryLinkID -> MagicHash -> SignatoryFieldsValuesForSigning -> [FileID] -> [String] -> Maybe ESignature -> Maybe String -> SignatoryScreenshots -> SignatoryConsentResponsesForSigning -> m ()
signDocument slid mh fields acceptedAuthorAttachments notUploadedSignatoryAttachments mesig mpin screenshots consentResponses = do
  switchLang =<< getLang <$> theDocument
  ctx <- getContext
  -- Note that the second 'guardGetSignatoryFromIdForDocument' call
  -- below may return a different result than the first one due to the
  -- field update, so don't attempt to replace the calls with a single
  -- call, or the actor identities may get wrong in the evidence log.
  fieldsWithFiles <- fieldsToFieldsWithFiles fields
  guardGetSignatoryFromIdForDocument slid >>= \sl -> dbUpdate . UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) =<< signatoryActor ctx sl
  guardGetSignatoryFromIdForDocument slid >>= \sl -> dbUpdate . UpdateConsentResponsesForSigning sl consentResponses =<< signatoryActor ctx sl
  theDocument >>= \doc -> do
    sl <- guardGetSignatoryFromIdForDocument slid
    authorAttachmentsWithAcceptanceText <- forM (documentauthorattachments doc) $ \a -> do
      acceptanceText <- renderTemplate "_authorAttachmentsUnderstoodContent" (F.value "attachment_name" $ authorattachmentname a)
      return (acceptanceText,a)
    notUploadedSignatoryAttachmentsText <- renderTemplate_ "_pageDocumentForAuthorHelpersLocalDialogsAttachmentmarkasnotuploaded"
    let notUploadedSignatoryAttachmentsWithText = zip notUploadedSignatoryAttachments (repeat notUploadedSignatoryAttachmentsText)
    dbUpdate . AddAcceptedAuthorAttachmentsEvents sl acceptedAuthorAttachments authorAttachmentsWithAcceptanceText =<< signatoryActor ctx sl
    dbUpdate . AddNotUploadedSignatoryAttachmentsEvents sl notUploadedSignatoryAttachmentsWithText =<< signatoryActor ctx sl
  guardGetSignatoryFromIdForDocument slid >>= \sl -> dbUpdate . SignDocument slid mh mesig mpin screenshots =<< signatoryActor ctx sl


fieldsToFieldsWithFiles :: (MonadDB m, MonadThrow m, MonadTime m) =>
  SignatoryFieldsValuesForSigning -> m ([(FieldIdentity,FieldValue)],[(FileID,BS.ByteString)])
fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning []) = return ([],[])
fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning (f:fs)) = do
  (changeFields,files') <- fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning fs)
  case f of
    (fi,StringFTV s) -> return ((fi,StringFV s):changeFields,files')
    (fi,BoolFTV b)   -> return ((fi,BoolFV b):changeFields,files')
    (fi,FileFTV bs)  -> if (BS.null bs)
      then return $ ((fi,FileFV Nothing):changeFields,files')
      else do
        fileid <- dbUpdate $ NewFile "signature.png" bs
        return $ ((fi,FileFV (Just fileid)):changeFields,(fileid,bs):files')


checkSignatoryPinToSign :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> SignatoryFieldsValuesForSigning -> String -> m Bool
checkSignatoryPinToSign slid (SignatoryFieldsValuesForSigning fields) pin = do
  sl <- guardGetSignatoryFromIdForDocument slid
  let mobileEditableBySignatory = Just True == join (fieldEditableBySignatory <$> getFieldByIdentity MobileFI (signatoryfields sl))
  let slidMobile = getMobile sl
  mobile <- case (not (null slidMobile) && not mobileEditableBySignatory , lookup MobileFI fields) of
    (True, _) -> return slidMobile
    (False, Just (StringFTV v)) -> return v
    (False, _) -> apiError $ requestParameterInvalid "fields"
                    "Does not contain a mobile number field, author has not set one for the signatory"
  pin' <- dbQuery $ GetSignatoryPin SMSPinToSign slid mobile
  return $ pin == pin'

checkSignatoryPinToView :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> String -> m Bool
checkSignatoryPinToView slid pin = do
  sl <- guardGetSignatoryFromIdForDocument slid
  pin' <- dbQuery $ GetSignatoryPin SMSPinToView slid (getMobile sl)
  return $ pin == pin'
