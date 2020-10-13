module Doc.API.V2.Calls.SignatoryCallsUtils (
  checkAuthenticationToSignMethodAndValue
, getScreenshots
, signDocument
, checkSignatoryPinToSign
, checkSignatoryPinToView
, fieldsToFieldsWithFiles
) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Time
import Crypto.RNG
import Log
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import API.V2
import API.V2.Parameters
import DB
import Doc.API.V2.Guards
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots
  ( SignatoryScreenshots, emptySignatoryScreenshots
  , resolveReferenceScreenshotNames
  )
import Doc.SMSPin.Model
import EID.Signature.Model
import File.Model
import File.Storage
import File.Types (fileid)
import InputValidation (Result(..), asValidPhoneForSMS)
import Kontra
import Templates (renderTextTemplate, renderTextTemplate_)
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Check if provided authorization values for sign call patch
checkAuthenticationToSignMethodAndValue
  :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
checkAuthenticationToSignMethodAndValue slid = do
  mAuthType <- apiV2ParameterOptional
    (ApiV2ParameterTextUnjson "authentication_type" unjsonAuthenticationToSignMethod)
  case mAuthType of
    Nothing         -> return ()
    Just authMethod -> do
      siglink <- guardGetSignatoryFromIdForDocument slid
      let siglinkSignMethod = signatorylinkauthenticationtosignmethod siglink
          authOK            = coerceLegacyDKNemIDMethod authMethod
            == coerceLegacyDKNemIDMethod siglinkSignMethod
      unless authOK . apiError $ requestParameterInvalid "authentication_type"
                                                         "does not match with document"
      when (authToSignNeedsPersonalNumber authMethod)
        $ checkParamSSNMatchesSigLink siglink
      when (authToSignNeedsMobileNumber authMethod)
        $ checkParamMobileMatchesSigLink siglink
  where
    checkParamMobileMatchesSigLink siglink = do
      authValue <- apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
      let mobileEditableBySignatory =
            Just True
              == (   fieldEditableBySignatory
                 =<< getFieldByIdentity MobileFI (signatoryfields siglink)
                 )
      if authValue
           == getMobile siglink
           || T.null (getMobile siglink)
           || mobileEditableBySignatory
        then return ()
        else apiError $ requestParameterInvalid "authentication_value"
                                                "value for mobile number does not match"
    checkParamSSNMatchesSigLink siglink = do
      authValue <- apiV2ParameterObligatory (ApiV2ParameterText "authentication_value")
      if authValue == getPersonalNumber siglink || T.null (getPersonalNumber siglink)
        then return ()
        else apiError $ requestParameterInvalid
          "authentication_value"
          "value for personal number does not match"
    coerceLegacyDKNemIDMethod :: AuthenticationToSignMethod -> AuthenticationToSignMethod
    coerceLegacyDKNemIDMethod = \case
      LegacyDKNemIDAuthenticationToSign -> DKNemIDCPRAuthenticationToSign
      m -> m

getScreenshots :: (Kontrakcja m) => m SignatoryScreenshots
getScreenshots = do
  screenshots <- apiV2ParameterDefault
    emptySignatoryScreenshots
    (ApiV2ParameterJSON "screenshots" unjsonSignatoryScreenshots)
  resolvedScreenshots <- resolveReferenceScreenshotNames screenshots
  case resolvedScreenshots of
    Nothing -> apiError
      $ requestParameterInvalid "screenshots" "Could not resolve reference screenshot"
    Just res -> return res

signDocument
  :: (Kontrakcja m, DocumentMonad m)
  => SignatoryLinkID
  -> SignatoryFieldsValuesForSigning
  -> [FileID]
  -> [Text]
  -> Maybe ESignature
  -> Maybe Text
  -> SignatoryScreenshots
  -> SignatoryConsentResponsesForSigning
  -> m ()
signDocument slid fields acceptedAuthorAttachments notUploadedSignatoryAttachments mesig mpin screenshots consentResponses
  = do
    switchLang =<< getLang <$> theDocument

    ctx             <- getContext
    fieldsWithFiles <- fieldsToFieldsWithFiles fields

    -- Note that the second 'guardGetSignatoryFromIdForDocument' call
    -- below may return a different result than the first one due to the
    -- field update, so don't attempt to replace the calls with a single
    -- call, or the actor identities may get wrong in the evidence log.
    guardGetSignatoryFromIdForDocument slid >>= \sl ->
      dbUpdate
        .   uncurry (UpdateFieldsForSigning sl) fieldsWithFiles
        =<< signatoryActor ctx sl

    guardGetSignatoryFromIdForDocument slid >>= \sl ->
      dbUpdate
        .   UpdateConsentResponsesForSigning sl consentResponses
        =<< signatoryActor ctx sl

    theDocument >>= \doc -> do
      sl <- guardGetSignatoryFromIdForDocument slid
      authorAttachmentsWithAcceptanceText <- forM (documentauthorattachments doc) $ \a ->
        do
          acceptanceText <- renderTextTemplate
            "_authorAttachmentsUnderstoodContent"
            (F.value "attachment_name" $ authorattachmentname a)
          return (acceptanceText, a)
      notUploadedSignatoryAttachmentsText <- renderTextTemplate_
        "_pageDocumentForAuthorHelpersLocalDialogsAttachmentmarkasnotuploaded"
      let notUploadedSignatoryAttachmentsWithText = zip
            notUploadedSignatoryAttachments
            (repeat notUploadedSignatoryAttachmentsText)
      dbUpdate
        .   AddAcceptedAuthorAttachmentsEvents sl
                                               acceptedAuthorAttachments
                                               authorAttachmentsWithAcceptanceText
        =<< signatoryActor ctx sl
      dbUpdate
        .   AddNotUploadedSignatoryAttachmentsEvents
              sl
              notUploadedSignatoryAttachmentsWithText
        =<< signatoryActor ctx sl

    guardGetSignatoryFromIdForDocument slid
      >>= signatoryActor ctx
      >>= dbUpdate
      .   SignDocument slid mesig mpin screenshots


fieldsToFieldsWithFiles
  :: ( CryptoRNG m
     , MonadBase IO m
     , MonadCatch m
     , MonadFileStorage m
     , MonadLog m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     )
  => SignatoryFieldsValuesForSigning
  -> m ([(FieldIdentity, FieldValue)], [(FileID, BS.ByteString)])
fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning []      ) = return ([], [])
fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning (f : fs)) = do
  (changeFields, files') <- fieldsToFieldsWithFiles (SignatoryFieldsValuesForSigning fs)
  case f of
    (fi, StringFTV s) -> return ((fi, StringFV s) : changeFields, files')
    (fi, BoolFTV b  ) -> return ((fi, BoolFV b) : changeFields, files')
    (fi, FileFTV bs ) -> if BS.null bs
      then return ((fi, FileFV Nothing) : changeFields, files')
      else do
        file <- saveNewFile "signature.png" bs
        return
          ((fi, FileFV . Just $ fileid file) : changeFields, (fileid file, bs) : files')


checkSignatoryPinToSign
  :: (Kontrakcja m, DocumentMonad m)
  => SignatoryLinkID
  -> SignatoryFieldsValuesForSigning
  -> Text
  -> m Bool
checkSignatoryPinToSign slid (SignatoryFieldsValuesForSigning fields) pin = do
  sl <- guardGetSignatoryFromIdForDocument slid
  let mobileEditableBySignatory =
        Just True
          == (   fieldEditableBySignatory
             =<< getFieldByIdentity MobileFI (signatoryfields sl)
             )
  let slidMobile = getMobile sl
  mobile <-
    case
      (not (T.null slidMobile) && not mobileEditableBySignatory, lookup MobileFI fields)
    of
      (True, _) -> case asValidPhoneForSMS slidMobile of
        Good v -> return v
        _ ->
          apiError $ serverError "Mobile number for signatory set by author is not valid"
      (False, Just (StringFTV v)) -> case asValidPhoneForSMS v of
        Good x -> return x
        _      -> apiError
          $ requestParameterInvalid "fields" "Does contain invalid mobile number field"
      (False, _) -> apiError $ requestParameterInvalid
        "fields"
        "Does not contain a mobile number field, author has not set one for the signatory"
  ctx   <- getContext
  actor <- signatoryActor ctx sl
  (== SignatoryPinCorrect)
    <$> dbUpdate (VerifySignatoryPin pin SMSPinToSign sl mobile actor)


checkSignatoryPinToView
  :: (Kontrakcja m, DocumentMonad m) => SMSPinType -> SignatoryLinkID -> Text -> m Bool
checkSignatoryPinToView pinType slid pin = do
  sl     <- guardGetSignatoryFromIdForDocument slid
  mobile <- case asValidPhoneForSMS $ getMobile sl of
    Good x -> return x
    _ -> apiError $ serverError "Mobile number for signatory set by author is not valid"
  ctx   <- getContext
  actor <- signatoryActor ctx sl
  (== SignatoryPinCorrect) <$> dbUpdate (VerifySignatoryPin pin pinType sl mobile actor)
