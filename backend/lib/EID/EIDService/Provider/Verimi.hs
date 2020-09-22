{-# LANGUAGE DuplicateRecordFields #-}
module EID.EIDService.Provider.Verimi (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
  , VerimiSignCompletionData(..)
  , VerimiSignCompletionDataFromResponse(..)
  , VerimiSignedPdf(..)
  , VerimiSignedDocuments (..)
  , eidServiceVerimiQesSignatureFromCompletionData
  ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import GHC.Generics
import qualified Text.StringTemplates.Fields as F

import Chargeable
import DB
import Doc.DocSeal (signatoryFieldNameForSignatory)
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import EvidenceLog.Model
import File.Storage
import Happstack.Fields
import Kontra hiding (InternalError)
import Session.Model
import Text.JSON.Convert (Base64ByteString, toBase64)
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderVerimi

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind = case authKind of
  EIDServiceAuthToView _ -> beginAuthTransaction conf authKind
  EIDServiceAuthToSign   -> beginSignTransaction conf


{- 1. Authentication (to view) -}
newtype StartVerimiAuthTransactionResponse = StartVerimiAuthTransactionResponse {
    authUrl :: Text
  }

instance FromJSON StartVerimiAuthTransactionResponse where
  parseJSON outer =
    StartVerimiAuthTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: eidServiceFieldName)
          >>= withObject "object" (.: "authUrl")
          )
    where eidServiceFieldName = toEIDServiceProviderName provider <> "Auth"

beginAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginAuthTransaction conf authKind doc sl = do
  redirectUrl <- do
    ctx          <- getContext
    postredirect <- guardJustM (getField "redirect")
    return $ showt UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                      , redProvider        = provider
                                      , redAuthKind        = authKind
                                      , redDocumentID      = documentid doc
                                      , redSignatoryLinkID = signatorylinkid sl
                                      , redPostRedirectUrl = Just postredirect
                                      }

  let createReq = CreateEIDServiceTransactionRequest { cestProvider = provider
                                                     , cestMethod = EIDServiceAuthMethod
                                                     , cestRedirectUrl = redirectUrl
                                                     , cestProviderParameters = Nothing
                                                     }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- authUrl <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CIVerimiAuthenticationStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data VerimiAuthCompletionData = VerimiAuthCompletionData
  { userId :: Text
  , name :: Maybe Text
  , email :: Maybe Text
  , emailVerified :: Maybe Bool
  , birthdate    :: Maybe Text
  , phoneNumber :: Maybe Text
  , phoneNumberVerified :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON VerimiAuthCompletionData where

newtype VerimiAuthCompletionDataFromResponse = VerimiAuthCompletionDataFromResponse VerimiAuthCompletionData
instance FromJSON VerimiAuthCompletionDataFromResponse where
  parseJSON outer =
    VerimiAuthCompletionDataFromResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: "verimiAuth")
          >>= withObject "object" (.: "completionData")
          )

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf doc sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToView $ mkAuthKind doc
  runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    let eidServiceStatus = estRespStatus trans
        dbStatus         = estStatus estDB
    if eidServiceStatus == dbStatus
      then return eidServiceStatus
      else finaliseTransaction estDB trans
  where
    validateCompletionData trans = do
      VerimiAuthCompletionDataFromResponse completionData <- estRespCompletionData trans
      guard $ email completionData == Just (getEmail sl)
      guard $ emailVerified completionData == Just True
      eIDServiceVerimiAuthenticationFromCompletionData completionData

    updateDBTransactionWithCompletionData cd = do
      let auth = EIDServiceVerimiAuthentication
            { eidServiceVerimiName          = eidServiceVerimiName cd
            , eidServiceVerimiVerifiedEmail = eidServiceVerimiVerifiedEmail cd
            , eidServiceVerimiVerifiedPhone = Nothing
            }
      sessionID <- getNonTempSessionID
      dbUpdate $ MergeEIDServiceVerimiAuthentication (mkAuthKind doc)
                                                     sessionID
                                                     (signatorylinkid sl)
                                                     auth

    updateEvidenceLog cd = do
      ctx <- getContext
      let eventFields = do
            F.value "signatory_name" $ eidServiceVerimiName cd
            F.value "provider_verimi" True
            F.value "provider_email" $ eidServiceVerimiVerifiedEmail cd
      withDocument doc
        .   void
        $   dbUpdate
        .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                           eventFields
                                                           (Just sl)
                                                           Nothing
        =<< signatoryActor ctx sl

    finaliseTransaction estDB trans = case validateCompletionData trans of
      Nothing -> do
        let status = EIDServiceTransactionStatusCompleteAndFailed
        mergeEIDServiceTransactionWithStatus status
        return status
      Just cd -> do
        let status = EIDServiceTransactionStatusCompleteAndSuccess
        mergeEIDServiceTransactionWithStatus status
        updateDBTransactionWithCompletionData cd
        updateEvidenceLog cd
        chargeForItemSingle CIVerimiAuthenticationFinished $ documentid doc
        return status
      where
        mergeEIDServiceTransactionWithStatus newstatus =
          dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }


{- 2. QES Signing -}
newtype StartVerimiSignTransactionResponse = StartVerimiSignTransactionResponse {
    signUrl :: Text
  }

instance FromJSON StartVerimiSignTransactionResponse where
  parseJSON outer =
    StartVerimiSignTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: "verimiSign")
          >>= withObject "object" (.: "signUrl")
          )

data VerimiSignParams = VerimiSignParams {
    userEmail :: Maybe Text
  , documentsToSign :: [VerimiDocumentCluster]
  } deriving Generic

data VerimiDocumentCluster = VerimiDocumentCluster {
    clusterTitle  :: Maybe Text
  , clusterOrder :: Int
  , files :: [VerimiDocumentFile]
  } deriving Generic

data VerimiDocumentFile = VerimiDocumentFile {
    documentId   :: Text
  , order        :: Int
  , documentData :: Base64ByteString
  , filename     :: Text
  , title        :: Maybe Text
  , description  :: Maybe Text
  , showDocument :: VerimiShowDocumentFlag
  , legalEntity  :: Maybe Text
  , signatures   :: [VerimiSignaturePlacement]
  } deriving Generic

data VerimiShowDocumentFlag = VerimiShowDocumentOptional | VerimiShowDocumentMandatory

data VerimiSignaturePlacement = VerimiSignaturePlacement {
    signatureFieldName :: Maybe Text
  , signaturePage      :: Maybe Int
  , signaturePosX      :: Maybe Int
  , signaturePosY      :: Maybe Int
  , signatureWidth     :: Maybe Int
  , signatureHeight    :: Maybe Int
  } deriving Generic


instance ToJSON VerimiSignaturePlacement where
instance ToJSON VerimiDocumentFile where
instance ToJSON VerimiDocumentCluster where
instance ToJSON VerimiSignParams where

instance ToJSON VerimiShowDocumentFlag where
  toJSON VerimiShowDocumentOptional  = String "OPTIONAL"
  toJSON VerimiShowDocumentMandatory = String "MANDATORY"

beginSignTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginSignTransaction conf doc sl = do
  redirectUrl <- do
    ctx <- getContext
    return $ showt UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                      , redProvider        = provider
                                      , redAuthKind        = EIDServiceAuthToSign
                                      , redDocumentID      = documentid doc
                                      , redSignatoryLinkID = signatorylinkid sl
                                      , redPostRedirectUrl = Nothing
                                      }

  mainfileContent <- case documentfile doc of
    Just PendingVerimiQesFile {..} -> getFileContents mainfileWithSomeQesSignatures
    _ -> unexpectedError "Error signing with Verimi QES: no PendingVerimiQesFile!"

  let
    createReq = CreateEIDServiceTransactionRequest
      { cestProvider           = provider
      , cestMethod             = EIDServiceSignMethod
      , cestRedirectUrl        = redirectUrl
      , cestProviderParameters =
        Just . toJSON $ VerimiSignParams
          { userEmail       = Just $ getEmail sl
          , documentsToSign =
            [ VerimiDocumentCluster
                { clusterOrder = 0
                , clusterTitle = Nothing
                , files        =
                  [ VerimiDocumentFile
                      { documentId   = "The only document!"
                      , order        = 0
                      , documentData = toBase64 mainfileContent
                      , filename     = documenttitle doc
                      , title        = Just $ documenttitle doc
                      , description  = Nothing
                      , showDocument = VerimiShowDocumentOptional
                      , legalEntity  = Nothing
                      , signatures   = [ VerimiSignaturePlacement
                                           (Just $ signatoryFieldNameForSignatory sl)
                                           Nothing
                                           Nothing
                                           Nothing
                                           Nothing
                                           Nothing
                                       ]
                      }
                  ]
                }
            ]
          }
      }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- signUrl <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CIVerimiQesSignatureStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data VerimiSignCompletionData = VerimiSignCompletionData {
    authData :: VerimiAuthCompletionData
  , signedDocuments :: VerimiSignedDocuments
  } deriving Generic

newtype VerimiSignedDocuments = VerimiSignedDocuments {
    pdfs :: [VerimiSignedPdf]
  } deriving Generic

data VerimiSignedPdf = VerimiSignedPdf {
    documentId :: Text
  , documentData :: Base64ByteString
  } deriving Generic

instance FromJSON VerimiSignedPdf
instance FromJSON VerimiSignedDocuments
instance FromJSON VerimiSignCompletionData

newtype VerimiSignCompletionDataFromResponse = VerimiSignCompletionDataFromResponse { parse :: VerimiSignCompletionData }
instance FromJSON VerimiSignCompletionDataFromResponse where
  parseJSON outer =
    VerimiSignCompletionDataFromResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: "verimiSign")
          >>= withObject "object" (.: "completionData")
          )

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  msuccess <- runMaybeT $ do
    sessionID <- getNonTempSessionID
    estDB     <- MaybeT . dbQuery $ GetEIDServiceTransactionGuardSessionID
      sessionID
      (signatorylinkid sl)
      EIDServiceAuthToSign
    trans <- MaybeT $ getTransactionFromEIDService conf provider (estID estDB)
    VerimiSignCompletionDataFromResponse completionData <-
      MaybeT . return $ estRespCompletionData trans
    return
      $  (estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess)
      && isJust (eidServiceVerimiQesSignatureFromCompletionData completionData)
  return $ fromMaybe False msuccess

eIDServiceVerimiAuthenticationFromCompletionData
  :: VerimiAuthCompletionData -> Maybe EIDServiceVerimiAuthentication
eIDServiceVerimiAuthenticationFromCompletionData VerimiAuthCompletionData {..} =
  case name of
    Just eidServiceVerimiName -> Just EIDServiceVerimiAuthentication
      { eidServiceVerimiVerifiedEmail = guard (emailVerified == Just True) *> email
      , eidServiceVerimiVerifiedPhone = guard (phoneNumberVerified == Just True)
        *> phoneNumber
      , ..
      }
    _ -> Nothing

eidServiceVerimiQesSignatureFromCompletionData
  :: VerimiSignCompletionData -> Maybe EIDServiceVerimiQesSignature
eidServiceVerimiQesSignatureFromCompletionData VerimiSignCompletionData { authData } =
  case eIDServiceVerimiAuthenticationFromCompletionData authData of
    Just EIDServiceVerimiAuthentication {..} -> Just EIDServiceVerimiQesSignature
      { eidServiceVerimiSigVerifiedEmail = eidServiceVerimiVerifiedEmail
      , eidServiceVerimiSigVerifiedPhone = eidServiceVerimiVerifiedPhone
      , eidServiceVerimiSigName          = eidServiceVerimiName
      }
    _ -> Nothing
