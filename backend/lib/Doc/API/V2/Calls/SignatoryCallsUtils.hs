module Doc.API.V2.Calls.SignatoryCallsUtils (
  checkAuthenticationToSignMethodAndValue
, getScreenshots
, signDocument
, checkSignatoryPin
, fieldsToFieldsWithFiles
) where

import Control.Monad.Catch
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import API.V2
import DB
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc
import Doc.API.V2.Parameters
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots(SignatoryScreenshots, emptySignatoryScreenshots, resolveReferenceScreenshotNames)
import Doc.SMSPin.Model
import EID.Signature.Model
import File.Model
import Kontra
import KontraPrelude
import MagicHash (MagicHash)
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

{- | Check if provided authorization values for sign call patch -}
checkAuthenticationToSignMethodAndValue :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
checkAuthenticationToSignMethodAndValue slid = do
  mAuthType <- apiV2ParameterOptional (ApiV2ParameterTextUnjson "authentication_type" unjsonAuthenticationToSignMethod)
  case mAuthType of
    Nothing -> return ()
    Just authMethod -> do
      siglink <- $fromJust . getSigLinkFor slid <$> theDocument
      let authOK = authMethod == signatorylinkauthenticationtosignmethod siglink
      when (not authOK) $
        apiError $ requestParameterInvalid "authentication_type" "does not match with document"
      case authMethod of
        StandardAuthenticationToSign -> return ()
        SEBankIDAuthenticationToSign -> do
          authValue <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
          if (authValue == getPersonalNumber siglink || null (getPersonalNumber siglink))
            then return ()
            else apiError $
              requestParameterInvalid "authentication_value" "value for personal number does not match"
        SMSPinAuthenticationToSign -> do
          authValue <- T.unpack <$> apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
          if (authValue == getMobile siglink || null (getMobile siglink))
            then return ()
            else apiError $
              requestParameterInvalid "authentication_value" "value for mobile number does not match"

getScreenshots :: (Kontrakcja m) => m SignatoryScreenshots
getScreenshots = do
  screenshots <- apiV2ParameterDefault emptySignatoryScreenshots (ApiV2ParameterJSON "screenshots" unjsonSignatoryScreenshots)
  resolvedScreenshots <- resolveReferenceScreenshotNames screenshots
  case resolvedScreenshots of
    Nothing -> apiError $ requestParameterInvalid "screenshots" "Could not resolve reference screenshot"
    Just res -> return res



signDocument :: (Kontrakcja m, DocumentMonad m) =>
  SignatoryLinkID -> MagicHash -> SignatoryFieldsValuesForSigning -> [FileID] -> Maybe ESignature -> Maybe String -> SignatoryScreenshots -> m ()
signDocument slid mh fields acceptedAuthorAttachments mesig mpin screenshots = do
  switchLang =<< getLang <$> theDocument
  ctx <- getContext
  -- Note that the second 'getSigLinkFor' call below may return a
  -- different result than the first one due to the field update, so
  -- don't attempt to replace the calls with a single call, or the
  -- actor identities may get wrong in the evidence log.
  fieldsWithFiles <- fieldsToFieldsWithFiles fields
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) =<< signatoryActor ctx sl
  theDocument >>= \doc -> do
    let sl = $fromJust (getSigLinkFor slid doc)
    authorAttachmetsWithAcceptanceText <- forM (documentauthorattachments doc) $ \a -> do
      acceptanceText <- renderTemplate "_authorAttachmentsUnderstoodContent" (F.value "attachment_name" $ authorattachmentname a)
      return (acceptanceText,a)
    dbUpdate . AddAcceptedAuthorAttachmentsEvents sl acceptedAuthorAttachments authorAttachmetsWithAcceptanceText =<< signatoryActor ctx sl
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . SignDocument slid mh mesig mpin screenshots =<< signatoryActor ctx sl


fieldsToFieldsWithFiles :: (MonadDB m, MonadThrow m) =>
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


checkSignatoryPin :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> SignatoryFieldsValuesForSigning -> String -> m Bool
checkSignatoryPin slid (SignatoryFieldsValuesForSigning fields) pin = do
  slidMobile <- getMobile <$> $fromJust . getSigLinkFor slid <$> theDocument
  mobile <- case (not $ null slidMobile, lookup MobileFI fields) of
    (True, _) -> return slidMobile
    (False, Just (StringFTV v)) -> return v
    (False, _) -> apiError $ requestParameterInvalid "fields"
                    "Does not contain a mobile number field, author has not set one for the signatory"
  pin' <- dbQuery $ GetSignatoryPin slid mobile
  return $ pin == pin'
