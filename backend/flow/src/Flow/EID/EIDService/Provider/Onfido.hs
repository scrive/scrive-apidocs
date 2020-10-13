module Flow.EID.EIDService.Provider.Onfido (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , OnfidoEIDServiceCompletionData(..)
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log
import qualified Data.Set as Set
import qualified Text.StringTemplates.Fields as F

import Chargeable
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model.Query
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Provider.Onfido
  ( CompletionData(..), OnfidoDocumentType(..)
  , OnfidoEIDServiceCompletionData(..), OnfidoEIDServiceProviderParams(..)
  , completionDataToName, eidonfidoDateOfBirth, validateCompletionData
  )
import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import EvidenceLog.Model
import Flow.ActionConsumers
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Provider.Common
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Names
import Flow.Utils
import Happstack.Fields
import Kontra hiding (InternalError)
import Log.Identifier
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import qualified Flow.CallbackPayload as CB
import qualified Flow.CallbackPayload as Callback
  ( AuthenticationProvider(..), AuthenticationProviderOnfido(..)
  )
import qualified Flow.Core.Type.AuthenticationConfiguration as Core

eidProvider :: EIDServiceTransactionProvider
eidProvider = EIDServiceTransactionProviderOnfido

toEidDocumentType :: Core.OnfidoDocumentType -> OnfidoDocumentType
toEidDocumentType = \case
  Core.NationalIdentityCard -> NationalIdentityCard
  Core.DrivingLicence       -> DrivingLicence
  Core.Passport             -> Passport
  Core.ResidencePermit      -> ResidencePermit

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> AuthenticationKind
  -> Core.AuthenticationProviderOnfidoData
  -> InstanceId
  -> UserName
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind onfidoAuthenticationData instanceId userName =
  do
    ctx            <- getContext
    kontraRedirect <- guardJustM (getField "redirect")
    let onfidoMethod = Core.method onfidoAuthenticationData
    let redirectUrl = UnifiedRedirectUrl { redDomain = ctx ^. #brandedDomain % #url
                                         , redProvider = eidProvider
                                         , redAuthKind = EIDServiceAuthToView authKind
                                         , redInstanceId = instanceId
                                         , redUserName = userName
                                         , redPostRedirectUrl = Just kontraRedirect
                                         }
    (did, slid) <- findFirstSignatoryLink instanceId userName
    sl          <- dbQuery $ GetSignatoryLinkByID did slid

    let methodParam = case onfidoMethod of
          Core.Document         -> OnfidoDocumentCheck
          Core.DocumentAndPhoto -> OnfidoDocumentAndPhotoCheck

    let documentTypes =
          Set.map toEidDocumentType $ Core.allowedDocumentTypes onfidoAuthenticationData

    let providerParams = OnfidoEIDServiceProviderParams
          { onfidoparamMethod               = methodParam
          , onfidoparamFirstName            = getFirstName sl
          , onfidoparamLastName             = getLastName sl
          , onfidoparamAllowedDocumentTypes = Just documentTypes
          }

    let createReq = CreateEIDServiceTransactionRequest
          { cestProvider           = eidProvider
          , cestMethod             = EIDServiceAuthMethod
          , cestRedirectUrl        = showt redirectUrl
          , cestProviderParameters = Just $ toJSON providerParams
          }
    -- Onfido transactions are not started from the API, we get the URL via the create call
    trans <- createTransactionWithEIDService conf createReq
    case onfidoMethod of
      Core.Document -> chargeForItemSingle CIOnfidoDocumentCheckAuthenticationStarted did
      Core.DocumentAndPhoto ->
        chargeForItemSingle CIOnfidoDocumentAndPhotoCheckAuthenticationStarted did
    let tid  = cestRespTransactionID trans
        turl = cestRespAccessUrl trans
    return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusNew)

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> InstanceId
  -> UserName
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf instanceId userName = runMaybeT $ do
  Just (authKind, estDB, trans) <- getTransactionForUser conf
                                                         eidProvider
                                                         instanceId
                                                         userName
  if estRespStatus trans == estStatus estDB
    then return $ estRespStatus trans
    else finaliseTransaction instanceId userName authKind estDB trans

finaliseTransaction
  :: Kontrakcja m
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse OnfidoEIDServiceCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction instanceId userName authKind estDB trans =
  case validateCompletionData trans of
    Nothing -> do
      let status = EIDServiceTransactionStatusCompleteAndFailed
      mergeEIDServiceTransactionWithStatus status
      insertAuthenticationFailure instanceId userName authKind
      case estRespCompletionData trans of
        Just cd -> sendCallback cd CB.Failure
        Nothing -> do
          logInfo "Finalizing transaction which is not finished"
            $ object [logPair_ instanceId]
          return ()
      return status
    Just cd -> do
      let status = EIDServiceTransactionStatusCompleteAndSuccess
      mergeEIDServiceTransactionWithStatus status
      updateDBTransactionWithCompletionData instanceId userName authKind cd
      when (authKind == AuthenticationToView)
        $ updateEvidenceLogForRelevantDocs instanceId userName cd
      sendCallback cd CB.Success
      return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }
    sendCallback cd result =
      sendEventCallback instanceId
        . CB.AuthenticationAttempted
        $ CB.AuthenticationAttemptedEvent
            { userName = userName
            , result   = result
            , provider = Callback.Onfido $ Callback.AuthenticationProviderOnfido
                           { applicantId = eidonfidoApplicantId cd
                           }
            }

updateDBTransactionWithCompletionData
  :: Kontrakcja m
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> OnfidoEIDServiceCompletionData
  -> m ()
updateDBTransactionWithCompletionData instanceId userName authKind OnfidoEIDServiceCompletionData {..}
  = do
    sessionID <- getNonTempSessionID
    updateFlowEidAuthentication instanceId userName authKind sessionID
      $ EIDServiceOnfidoAuthentication_ EIDServiceOnfidoAuthentication
          { eidServiceOnfidoSignatoryName = completionDataToName eidonfidoCompletionData
          , eidServiceOnfidoDateOfBirth   = eidonfidoDateOfBirth eidonfidoCompletionData
          , eidServiceOnfidoMethod        = eidonfidoMethod
          }

updateEvidenceLogForRelevantDocs
  :: Kontrakcja m => InstanceId -> UserName -> OnfidoEIDServiceCompletionData -> m ()
updateEvidenceLogForRelevantDocs instanceId userName cd = do
  ctx    <- getContext
  slDocs <- getSignatoriesAndDocumentsForUser instanceId userName
  forM_ slDocs $ \(sl, doc) -> do
    let eventFields = do
          F.value "signatory_name" . eidonfidoLastName $ eidonfidoCompletionData cd
          F.value "signatory_dob" . eidonfidoDateOfBirth $ eidonfidoCompletionData cd
          F.value "flow_auth_to_view" True
          F.value "provider_onfido" True
          case eidonfidoMethod cd of
            OnfidoDocumentCheck         -> F.value "onfido_document" True
            OnfidoDocumentAndPhotoCheck -> F.value "onfido_document_and_photo_check" True
    withDocument doc
      .   void
      $   dbUpdate
      .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                         eventFields
                                                         (Just sl)
                                                         Nothing
      =<< signatoryActor ctx sl
