module Doc.API.V2.Calls.SignatoryCallsUtils (
  checkAuthenticationMethodAndValue
, getScreenshots
, signDocument
, checkSignatoryPin
) where

import Data.Text (pack)
import Data.Unjson
import Happstack.Fields
import qualified Data.ByteString.Char8 as BS

import API.V2
import DB
import Doc.API.V2.JSONFields
import Doc.API.V2.JSONMisc
import Doc.API.V2.Parameters
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model.Update
import Doc.SMSPin.Model
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots(SignatoryScreenshots, emptySignatoryScreenshots, resolveReferenceScreenshotNames)
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
checkAuthenticationMethodAndValue :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
checkAuthenticationMethodAndValue slid = do
  mAuthType  :: Maybe String <- getField "authentication_type"
  mAuthValue :: Maybe String <- getField "authentication_value"
  case (mAuthType, mAuthValue) of
       (Just authType, Just authValue) -> do
           case (textToAuthenticationMethod $ pack authType) of
                Just authMethod -> do
                    siglink <- $fromJust . getSigLinkFor slid <$> theDocument
                    let authOK = authMethod == signatorylinkauthenticationmethod siglink
                    case (authOK, authMethod) of
                         (False, _) -> apiError $
                             requestParameterInvalid "authentication_type" "does not match with document"
                         (True, StandardAuthentication) -> return ()
                         (True, ELegAuthentication)   ->
                             if (authValue == getPersonalNumber siglink || null (getPersonalNumber siglink))
                                then return ()
                                else apiError $
                                    requestParameterInvalid "authentication_value" "value for personal number does not match"
                         (True, SMSPinAuthentication) ->
                             if (authValue == getMobile siglink || null (getMobile siglink))
                                then return ()
                                else apiError $
                                    requestParameterInvalid "authentication_value" "value for phone number does not match"
                Nothing ->
                    apiError $ requestParameterParseError "authentication_type" "invalid type"
       (Nothing, Nothing) -> return ()
       (Just _, Nothing) ->  apiError $ requestParameterMissing "authentication_value"
       (Nothing, Just _) ->  apiError $ requestParameterMissing "authentication_type"

getScreenshots :: (Kontrakcja m) => m SignatoryScreenshots
getScreenshots = do
  screenshots <- apiV2Parameter' (ApiV2ParameterJSON "screenshots" (OptionalWithDefault emptySignatoryScreenshots) unjsonDef)
  resolvedScreenshots <- resolveReferenceScreenshotNames screenshots
  case resolvedScreenshots of
    Nothing -> apiError $ requestParameterInvalid "screenshots" "Could not resolve reference screenshot"
    Just res -> return res



signDocument :: (Kontrakcja m, DocumentMonad m)
             => SignatoryLinkID
             -> MagicHash
             -> [(FieldIdentity, SignatoryFieldTMPValue)]
             -> Maybe ESignature
             -> Maybe String
             -> SignatoryScreenshots
             -> m ()
signDocument slid mh fields mesig mpin screenshots = do
  switchLang =<< getLang <$> theDocument
  ctx <- getContext
  -- Note that the second 'getSigLinkFor' call below may return a
  -- different result than the first one due to the field update, so
  -- don't attempt to replace the calls with a single call, or the
  -- actor identities may get wrong in the evidence log.
  fieldsWithFiles <- fieldsToFieldsWithFiles fields
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) =<< signatoryActor ctx sl
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . SignDocument slid mh mesig mpin screenshots =<< signatoryActor ctx sl


fieldsToFieldsWithFiles :: (Kontrakcja m)
                           => [(FieldIdentity,SignatoryFieldTMPValue)]
                           -> m ([(FieldIdentity,FieldValue)],[(FileID,BS.ByteString)])
fieldsToFieldsWithFiles [] = return ([],[])
fieldsToFieldsWithFiles (f:fs) = do
  (changeFields,files') <- fieldsToFieldsWithFiles fs
  case f of
    (fi,StringFTV s) -> return ((fi,StringFV s):changeFields,files')
    (fi,BoolFTV b)   -> return ((fi,BoolFV b):changeFields,files')
    (fi,FileFTV bs)  -> if (BS.null bs)
                          then return $ ((fi,FileFV Nothing):changeFields,files')
                          else do
                            fileid <- dbUpdate $ NewFile "signature.png" (Binary bs)
                            return $ ((fi,FileFV (Just fileid)):changeFields,(fileid,bs):files')


checkSignatoryPin :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> [(FieldIdentity, SignatoryFieldTMPValue)] -> String -> m Bool
checkSignatoryPin slid fields pin = do
  phone <- case (lookup MobileFI fields) of
    Just (StringFTV v) -> return v
    _ ->  getMobile <$> $fromJust . getSigLinkFor slid <$> theDocument
  pin' <- dbQuery $ GetSignatoryPin slid phone
  return $ pin == pin'
