module EID.EIDService.Provider.Onfido (
    OnfidoEIDServiceCompletionData(..)
  , CompletionData(..)
  , beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
  , completionDataToName
  , OnfidoEIDServiceProviderParams(..)
  , validateCompletionData
  , OnfidoDocumentType(..)
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Set (Set)
import GHC.Generics
import Log
import qualified Text.StringTemplates.Fields as F

import Chargeable
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import EvidenceLog.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderOnfido

data OnfidoDocumentType
  = NationalIdentityCard
  | DrivingLicence
  | Passport
  | ResidencePermit
  deriving (Eq, Generic, Ord)

instance ToJSON OnfidoDocumentType where
  toJSON = genericToJSON defaultOptions

data OnfidoEIDServiceProviderParams = OnfidoEIDServiceProviderParams {
    onfidoparamMethod :: OnfidoMethod
  , onfidoparamFirstName :: Text
  , onfidoparamLastName :: Text
  , onfidoparamAllowedDocumentTypes :: Maybe (Set OnfidoDocumentType)
  }

instance ToJSON OnfidoEIDServiceProviderParams where
  toJSON OnfidoEIDServiceProviderParams {..} = object
    [ "report" .= onfidoparamMethod
    , "firstName" .= onfidoparamFirstName
    , "lastName" .= onfidoparamLastName
    , "allowedDocumentTypes" .= onfidoparamAllowedDocumentTypes
    ]

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
  onfidoMethod <- case authKind of
    EIDServiceAuthToView AuthenticationToView ->
      authenticationMethodToOnfidoMethod $ signatorylinkauthenticationtoviewmethod sl
    EIDServiceAuthToView AuthenticationToViewArchived ->
      authenticationMethodToOnfidoMethod
        $ signatorylinkauthenticationtoviewarchivedmethod sl
    EIDServiceAuthToSign -> case signatorylinkauthenticationtosignmethod sl of
      OnfidoDocumentCheckAuthenticationToSign -> return OnfidoDocumentCheck
      OnfidoDocumentAndPhotoCheckAuthenticationToSign ->
        return OnfidoDocumentAndPhotoCheck
      method -> handleIncorrectMethod method
  ctx             <- getContext
  mkontraRedirect <- case authKind of
    EIDServiceAuthToView _ -> Just <$> guardJustM (getField "redirect")
    EIDServiceAuthToSign   -> return Nothing
  let redirectUrl = UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                       , redProvider        = provider
                                       , redAuthKind        = authKind
                                       , redDocumentID      = documentid doc
                                       , redSignatoryLinkID = signatorylinkid sl
                                       , redPostRedirectUrl = mkontraRedirect
                                       }
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just . toJSON $ OnfidoEIDServiceProviderParams
                                     { onfidoparamMethod               = onfidoMethod
                                     , onfidoparamFirstName            = getFirstName sl
                                     , onfidoparamLastName             = getLastName sl
                                     , onfidoparamAllowedDocumentTypes = Nothing
                                     }
        }
  -- Onfido transactions are not started from the API, we get the URL via the create call
  trans <- createTransactionWithEIDService conf createReq
  case (authKind, onfidoMethod) of
    (EIDServiceAuthToView _, OnfidoDocumentCheck) ->
      chargeForItemSingle CIOnfidoDocumentCheckAuthenticationStarted $ documentid doc
    (EIDServiceAuthToView _, OnfidoDocumentAndPhotoCheck) ->
      chargeForItemSingle CIOnfidoDocumentAndPhotoCheckAuthenticationStarted
        $ documentid doc
    (EIDServiceAuthToSign, OnfidoDocumentCheck) ->
      chargeForItemSingle CIOnfidoDocumentCheckSignatureStarted $ documentid doc
    (EIDServiceAuthToSign, OnfidoDocumentAndPhotoCheck) ->
      chargeForItemSingle CIOnfidoDocumentAndPhotoCheckSignatureStarted $ documentid doc
  let tid  = cestRespTransactionID trans
      turl = cestRespAccessUrl trans
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusNew)
  where
    handleIncorrectMethod method = do
      logInfo_
        $  "Tried to start Onfido transaction, but signatory was supposed to use "
        <> showt method
        <> "."
      internalError

    authenticationMethodToOnfidoMethod = \case
      OnfidoDocumentCheckAuthenticationToView -> return OnfidoDocumentCheck
      OnfidoDocumentAndPhotoCheckAuthenticationToView ->
        return OnfidoDocumentAndPhotoCheck
      method -> handleIncorrectMethod method

data CompletionData = CompletionData
  { eidonfidoFirstName   :: Text
  , eidonfidoLastName    :: Text
  , eidonfidoDateOfBirth :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON CompletionData where
  parseJSON cd =
    do
        withObject "object" (.: "documentReportData") cd
      >>= withObject
            "object"
            (\o -> do
              firstname <- o .: "firstName"
              lastname  <- o .: "lastName"
              dob       <- o .: "dateOfBirth"
              return CompletionData { eidonfidoFirstName   = firstname
                                    , eidonfidoLastName    = lastname
                                    , eidonfidoDateOfBirth = dob
                                    }
            )

data OnfidoEIDServiceCompletionData = OnfidoEIDServiceCompletionData
  { eidonfidoCompletionData :: CompletionData
  , eidonfidoMethod         :: OnfidoMethod
  , eidonfidoApplicantId    :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON OnfidoEIDServiceCompletionData where
  parseJSON outer = do
    onfidoMethod <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: toEIDServiceProviderName provider)
      >>= withObject "object" (.: "report")
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: eidServiceFieldName)
      >>= \o -> do
            completionData <- o .: "completionData"
            applicantId    <- o .: "applicantId"
            pure $ OnfidoEIDServiceCompletionData
              { eidonfidoCompletionData = completionData
              , eidonfidoMethod         = onfidoMethod
              , eidonfidoApplicantId    = applicantId
              }
    where eidServiceFieldName = toEIDServiceProviderName provider <> "Auth"

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf doc sl = do
  _sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToView $ mkAuthKind doc
  runMaybeT $ do
    Just estDB <- dbQuery
    -- TODO for CORE-2417: revert to guarding session ID, once the frontend part of Onfido
    -- auth-to-view is implemented and things can be tested directly.
    --  $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
      $ GetEIDServiceTransactionNoSessionIDGuard (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    logInfo_ $ "EID transaction: " <> showt trans
    let eidServiceStatus = estRespStatus trans
        dbStatus         = estStatus estDB
    if eidServiceStatus == dbStatus
      then return eidServiceStatus
      else finaliseTransaction doc sl estDB trans

finaliseTransaction
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse OnfidoEIDServiceCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = case validateCompletionData trans of
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status = EIDServiceTransactionStatusCompleteAndSuccess
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl cd
    updateEvidenceLog doc sl cd
    case eidonfidoMethod cd of
      OnfidoDocumentCheck ->
        chargeForItemSingle CIOnfidoDocumentCheckAuthenticationFinished $ documentid doc
      OnfidoDocumentAndPhotoCheck ->
        chargeForItemSingle CIOnfidoDocumentAndPhotoCheckAuthenticationFinished
          $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

validateCompletionData
  :: EIDServiceTransactionResponse OnfidoEIDServiceCompletionData
  -> Maybe OnfidoEIDServiceCompletionData
validateCompletionData trans = case (estRespStatus trans, estRespCompletionData trans) of
  (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> Just cd
  (_, _      ) -> Nothing

completionDataToName :: CompletionData -> Text
completionDataToName CompletionData {..} = eidonfidoFirstName <> " " <> eidonfidoLastName

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> OnfidoEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl OnfidoEIDServiceCompletionData {..} = do
  sessionID <- getNonTempSessionID
  dbUpdate
    . MergeDocumentEidAuthentication (mkAuthKind doc) sessionID (signatorylinkid sl)
    $ EIDServiceOnfidoAuthentication_ EIDServiceOnfidoAuthentication
        { eidServiceOnfidoSignatoryName = completionDataToName eidonfidoCompletionData
        , eidServiceOnfidoDateOfBirth   = eidonfidoDateOfBirth eidonfidoCompletionData
        , eidServiceOnfidoMethod        = eidonfidoMethod
        }

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return (trans :: EIDServiceTransactionResponse OnfidoEIDServiceCompletionData)
  case mtrans of
    Nothing -> return False
    Just trans ->
      return $ isSuccessFullTransaction trans && isJust (estRespCompletionData trans)
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> OnfidoEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl cd = do
  ctx <- getContext
  let eventFields = do
        F.value "signatory_name" . eidonfidoLastName $ eidonfidoCompletionData cd
        F.value "signatory_dob" . eidonfidoDateOfBirth $ eidonfidoCompletionData cd
        F.value "provider_onfido" True
  withDocument doc
    .   void
    $   dbUpdate
    .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                       eventFields
                                                       (Just sl)
                                                       Nothing
    =<< signatoryActor ctx sl
