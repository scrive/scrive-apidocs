module Flow.EID.EIDService.Control (
    eidServiceRoutes
  ) where

import Control.Monad.Extra
import Data.Aeson (Value)
import Happstack.Server hiding (Expired, dir)
import Happstack.StaticRouting
import Log
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Analytics.Include
import API.V2.Errors
import API.V2.MonadUtils
import AppView
import DB
import Doc.Model.Query
import EID.EIDService.Conf
import EID.EIDService.Model (eidServiceConf)
import EID.EIDService.Types hiding (EIDServiceTransactionFromDB(..))
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Provider
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Model
import Flow.Model.InstanceSession
import Flow.Model.Types
import Flow.Model.Types.Internal
import Flow.Names
import Happstack.Fields
import Kontra hiding (InternalError)
import MinutesTime
import Routing
import Session.Model
import Templates (renderTextTemplate)
import Util.MonadUtils
import qualified Context.Internal as Context

eidServiceRoutes :: Route (Kontra Response)
eidServiceRoutes = choice
  -- start endpoints
  [ dir "start" . hPost . toK3 $ startEIDServiceTransaction
  -- redirect endpoints
  , dir "redirect-endpoint" . hGet . toK3 $ redirectEndpointFromEIDServiceTransaction
  ]

-- newtype UserName here so that we don't taint flow code with Happstack
newtype LocalUserName = LocalUserName UserName

instance FromReqURI LocalUserName where
  fromReqURI = Just . LocalUserName . unsafeName . T.pack

startEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> InstanceId
  -> LocalUserName
  -> m Value
startEIDServiceTransaction provider instanceId (LocalUserName userName) = do
  logInfo "EID Service transaction start" $ object
    [ "instance_id" .= instanceId
    , "instance_user" .= userName
    , "provider" .= toEIDServiceProviderName provider
    ]

  guardSessionInstance instanceId userName
  fullInstance <- fromJust <$> selectFullInstance instanceId
  uac          <- fromMaybeM throwNoAuthentication
    $ selectUserAuthenticationConfiguration instanceId userName
  (authenticationKind, authenticationConfig) <-
    maybe throwNoAuthentication pure
      $ getAuthenticationKindAndConfiguration fullInstance uac

  maxFailuresExceeded <- checkAuthMaxFailuresExceeded
    instanceId
    userName
    (authenticationKind, authenticationConfig)
  when maxFailuresExceeded
    $ Kontra.unauthorized "Maximum number of authentication attempts exceeded."

  conf               <- getEidServiceConfiguration instanceId
  (tid, val, status) <- beginEIDServiceTransaction conf
                                                   provider
                                                   authenticationKind
                                                   authenticationConfig
                                                   instanceId
                                                   userName
  now <- currentTime
  sid <- getNonTempSessionID
  let newTransaction = EIDServiceTransactionFromDB
        { estID         = tid
        , estStatus     = status
        , estInstanceId = instanceId
        , estUserName   = userName
        , estAuthKind   = EIDServiceAuthToView authenticationKind
        , estProvider   = provider
        , estSessionID  = sid
        , estDeadline   = 60 `minutesAfter` now
        }
  dbUpdate $ MergeEIDServiceTransaction newTransaction
  return val
  where
    throwNoAuthentication :: Kontrakcja m => m a
    throwNoAuthentication = do
      let msg = "Participant doesn't have authentication configured."
      logAttention msg $ object ["instance_id" .= instanceId, "user_name" .= userName]
      apiError $ conflictError msg

redirectEndpointFromEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceTransactionProvider
  -> InstanceId
  -> LocalUserName
  -> m Response
redirectEndpointFromEIDServiceTransaction provider instanceId (LocalUserName userName) =
  do
    logInfo "EID Service transaction check" $ object
      [ "instance_id" .= instanceId
      , "instance_user" .= userName
      , "provider" .= toEIDServiceProviderName provider
      ]

    guardSessionInstance instanceId userName
    conf <- getEidServiceConfiguration instanceId

    ad   <- getAnalyticsData
    ctx  <- getContext
    rd   <- guardJustM $ getField "redirect"
    mts  <- completeEIDServiceAuthTransaction conf provider instanceId userName
    (simpleHtmlResponse =<<) . renderTextTemplate "postEIDAuthRedirect" $ do
      F.value "redirect" rd
      F.value "incorrect_data" (mts == Just EIDServiceTransactionStatusCompleteAndFailed)
      standardPageFields ctx Nothing ad

guardSessionInstance :: Kontrakcja m => InstanceId -> UserName -> m ()
guardSessionInstance instanceId' userName' = do
  (Context.sessionID <$> getContext) >>= selectInstanceSession >>= \case
    Nothing -> apiError $ APIError
      { errorType     = InvalidAuthorization
      , errorHttpCode = 401
      , errorMessage  = "Use access link to authenticate first."
      }
    Just InstanceSession {..} -> do
      when (instanceId /= instanceId')
        . apiError
        $ serverError
            "Flow instance parameter does not match the session data. Please update the session by using the correct Flow access link and try again."
      when (userName /= userName')
        . apiError
        $ serverError
            "Flow user name parameter does not match the session data. Please update the session by using the correct Flow access link and try again."

getEidServiceConfiguration :: Kontrakcja m => InstanceId -> m EIDServiceConf
getEidServiceConfiguration instanceId = do
  mDocumentId <- listToMaybe <$> selectDocumentIdsByInstanceId instanceId
  documentId  <- case mDocumentId of
    Nothing ->
      apiError $ serverError "Flow instance doesn't have any documents assigned."
    Just documentId -> pure documentId

  document <- dbQuery $ GetDocumentByDocumentID documentId
  eidServiceConf document
