module Flow.EID.EIDService.Provider.Onfido (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , OnfidoEIDServiceCompletionData(..)
 --  , completeEIDServiceAuthTransaction
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log

import Chargeable
import DB
import Doc.DocStateData
import Doc.Model.Query
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Provider.Onfido
  ( OnfidoEIDServiceCompletionData(..), OnfidoEIDServiceProviderParams(..)
  , completionDataToName, eidonfidoDateOfBirth, validateCompletionData
  )
import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import Flow.ActionConsumers
import Flow.Aggregator
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Model.Types
import Flow.Names
import Happstack.Fields
import Kontra hiding (InternalError)
import Log.Identifier
import Session.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
import qualified Flow.CallbackPayload as CB
import qualified Flow.CallbackPayload as Callback
  ( AuthenticationProvider(..), AuthenticationProviderOnfido(..)
  )
import qualified Flow.Core.Type.AuthenticationConfiguration as Core
import qualified Flow.Model as Model

eidProvider :: EIDServiceTransactionProvider
eidProvider = EIDServiceTransactionProviderOnfido

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
    -- TODO Locally `redDomain` contains localhost:8000 but we want :8888
    let onfidoMethod = Core.method onfidoAuthenticationData
    let redirectUrl = UnifiedRedirectUrl { redDomain = ctx ^. #brandedDomain % #url
                                         , redProvider = eidProvider
                                         , redAuthKind = EIDServiceAuthToView authKind
                                         , redInstanceId = instanceId
                                         , redUserName = userName
                                         , redPostRedirectUrl = Just kontraRedirect
                                         }
    -- TODO: Temporary - refactor into function when FLOW-325 is merged
    signatoryInfo <- find (\(userName', _, _) -> userName' == userName)
      <$> Model.selectSignatoryInfo instanceId
    (sl, did) <- case signatoryInfo of
      Just (_, slid, did) -> fmap (, did) <$> dbQuery $ GetSignatoryLinkByID did slid
      Nothing -> unexpectedError "beginEIDServiceTransaction: signatory info not found!"
    -- TODO: debugging
    logInfo_ $ "Redirect URL: " <> showt redirectUrl

    let createReq = CreateEIDServiceTransactionRequest
          { cestProvider           = eidProvider
          , cestMethod             = EIDServiceAuthMethod
          , cestRedirectUrl        = showt redirectUrl
          , cestProviderParameters = Just . toJSON $ OnfidoEIDServiceProviderParams
                                       { onfidoparamMethod    =
                                         case onfidoMethod of
                                           Core.Document -> OnfidoDocumentCheck
                                           Core.DocumentAndPhoto ->
                                             OnfidoDocumentAndPhotoCheck
                                       , onfidoparamFirstName = getFirstName sl
                                       , onfidoparamLastName  = getLastName sl
                                       }
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
completeEIDServiceAuthTransaction conf instanceId userName = do
  _sessionID   <- getNonTempSessionID
  fullInstance <- fromJust <$> Model.selectFullInstance instanceId
  let authKind = if isComplete $ instanceToAggregator fullInstance
        then AuthenticationToViewArchived
        else AuthenticationToView
  runMaybeT $ do
    Just estDB <-
      dbQuery
      . GetEIDServiceTransactionNoSessionIDGuard instanceId userName
      $ EIDServiceAuthToView authKind
    Just trans <- getTransactionFromEIDService conf eidProvider (estID estDB)
    logInfo_ $ "EID transaction: " <> showt trans
    let eidServiceStatus = estRespStatus trans
        dbStatus         = estStatus estDB
    if eidServiceStatus == dbStatus
      then return eidServiceStatus
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
