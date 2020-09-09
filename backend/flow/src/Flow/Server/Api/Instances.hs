module Flow.Server.Api.Instances where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Reader
import Data.Aeson
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple.Extra
import Log.Class
import Optics
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set

import AccessControl.Check
import AccessControl.Types
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Aggregator as Aggregator
import Flow.Core.Type.Url
import Flow.Engine
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types
import Flow.OrphanInstances ()
import Flow.Routes.Api as Api
import Flow.Routes.Types
import Flow.Server.Api.Common
import Flow.Server.Types
import Flow.Server.Utils
import KontraLink
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.Model.Types as Model

accountEndpoints :: ServerT (AuthProtect "account" :> InstanceApi) AppM
accountEndpoints account = getInstance account :<|> listInstances account

getInstance :: Account -> InstanceId -> AppM GetInstance
getInstance account instanceId = do
  logInfo_ "Getting instance"
  flowInstance <- checkInstancePerms account instanceId ReadA
  keyValues    <- Model.selectInstanceKeyValues instanceId
  authConfigs  <- Model.selectUserAuthConfigs instanceId
  accessTokens <- Model.selectInstanceAccessTokens instanceId
  let usersWithHashes = fmap (\act -> (act ^. #userName, act ^. #hash)) accessTokens
  let accessLinks     = mkAccessLinks (baseUrl account) instanceId usersWithHashes
  fullInstance <- fromMaybeM throwInstanceNotFoundError
    $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  tongue <- decodeHighTongueM $ fullInstance ^. #template % #process
  let mAvailableActions =
        mAllowedEvents tongue aggrState >>= (mapM (toAuthorAction keyValues) . Set.toList)
  let mkGetInstance availableActions status = GetInstance
        { id                 = instanceId
        , templateId         = flowInstance ^. #templateId
        , templateParameters = toTemplateParameters keyValues authConfigs
        , title              = flowInstance ^. #title
        -- TODO add a proper instance state
        , state              = InstanceState { availableActions }
        , accessLinks
        , status
        , started            = flowInstance ^. #started
        , lastEvent          = flowInstance ^. #lastEvent
        , callback           = flowInstance ^. #callback
        }

  if
    | currentStage aggrState == failureStageName -> pure $ mkGetInstance [] Failed
    | currentStage aggrState == finalStageName -> pure $ mkGetInstance [] Completed
    | otherwise -> case mAvailableActions of
      Just availableActions -> pure $ mkGetInstance availableActions InProgress
      _                     -> inconsistentInstanceState instanceId account
  where
    toAuthorAction :: InstanceKeyValues -> EventInfo -> Maybe InstanceAuthorAction
    toAuthorAction keyValues EventInfo {..} = do
      actionType     <- toApiUserAction eventInfoAction
      actionUser     <- keyValues ^. #users % at eventInfoUser
      actionDocument <- keyValues ^. #documents % at eventInfoDocument
      pure InstanceAuthorAction { actionType, actionUser, actionDocument }

    toTemplateParameters :: InstanceKeyValues -> [UserAuthConfig] -> TemplateParameters
    toTemplateParameters (InstanceKeyValues documents users messages) authConfigs =
      TemplateParameters documents (Map.mapWithKey toUserConfig users) messages
      where
        toUserConfig userName flowUserId =
          let uac = lookupUserAuthConfig userName authConfigs
          in  UserConfig { flowUserId
                         , authToView         = view #authToView =<< uac
                         , authToViewArchived = view #authToViewArchived =<< uac
                         }
        lookupUserAuthConfig userName = find
          (\uac -> uac ^. #userName == userName && uac ^. #instanceId == instanceId)

listInstances :: Account -> AppM [GetInstance]
listInstances account@Account {..} = do
  is <- Model.selectInstancesByUserID $ user ^. #id
  let iids = map (view #id) is
  mapM (getInstance account) iids

mAllowedEvents :: HighTongue -> AggregatorState -> Maybe (Set EventInfo)
mAllowedEvents HighTongue {..} aggrState@AggregatorState {..} =
  Set.filter (`Set.notMember` Aggregator.receivedEvents aggrState)
    .   fold
    .   Set.map (Set.fromList . expectToSuccess)
    .   stageExpect
    .   fst
    <$> remainingStages currentStage stages


inconsistentInstanceState
  :: (MonadLog m, MonadThrow m, MonadError ServerError m, ToJSON b)
  => InstanceId
  -> b
  -> m a
inconsistentInstanceState instanceId user = do
  logAttention "GetInstanceView: Invalid userId or broken state for Flow instance "
    $ object ["instance_id" .= instanceId, "instance_user" .= user]
  throwInternalServerError "Could not reconstruct the state of the Flow process."

toApiUserAction :: Machinize.UserAction -> Maybe Api.InstanceEventAction
toApiUserAction Machinize.Approval  = Just Api.Approve
toApiUserAction Machinize.Signature = Just Api.Sign
toApiUserAction Machinize.View      = Just Api.View
toApiUserAction Machinize.Rejection = Just Api.Reject
toApiUserAction (Machinize.Field _) = Nothing
toApiUserAction Machinize.Timeout   = Nothing


getInstanceView
  :: InstanceUser -> InstanceId -> Maybe Host -> IsSecure -> AppM GetInstanceView
getInstanceView user@InstanceUser {..} instanceId' mHost isSecure = do
  logInfo "Getting instance view"
    $ object ["instance_id" .= instanceId', "instance_user" .= user]

  when (instanceId /= instanceId') $ throwAuthenticationError AccessControlError

  FlowContext {..} <- ask
  let baseUrl = mkBaseUrl mainDomainUrl (isSecure == Secure) mHost

  keyValues    <- Model.selectInstanceKeyValues instanceId
  sigInfo      <- Model.selectSignatoryInfo instanceId
  fullInstance <- fromMaybeM throwInstanceNotFoundError
    $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  tongue <- decodeHighTongueM $ fullInstance ^. #template % #process

  -- Document state
  -- TODO Flow: This works because there is at most one event per document
  -- for every user. We should get relevant document for given users and
  -- compute state of the document by folding over all events.
  let userReceivedEvents =
        filter (\e -> eventInfoUser e == userName) . fmap toEventInfo $ allEvents
          fullInstance
      userDocs          = map eventInfoDocument userReceivedEvents
      userDocsWithState = mapMaybe
        (toApiUserDocument sigInfo userReceivedEvents $ keyValues ^. #documents)
        userDocs
      flowInstance = fullInstance ^. #flowInstance
      mkGetInstanceView actions status = GetInstanceView
        { id        = instanceId
        , title     = flowInstance ^. #title
        , state     = InstanceUserState { Api.documents = userDocsWithState }
        , actions
        , status
        , started   = flowInstance ^. #started
        , lastEvent = flowInstance ^. #lastEvent
        }

  if
    | currentStage aggrState == failureStageName -> pure $ mkGetInstanceView [] Failed
    | currentStage aggrState == finalStageName -> pure $ mkGetInstanceView [] Completed
    | otherwise -> case mAllowedEvents tongue aggrState of
      Just allowedEvents ->
        let
            -- Actions
            userAllowedEvents =
                Set.filter (\e -> eventInfoUser e == userName) allowedEvents
            userActions = catMaybes . Set.toList $ Set.map
              (\e -> do
                userAction <- toApiUserAction $ eventInfoAction e
                docId      <- Map.lookup (eventInfoDocument e) (keyValues ^. #documents)
                sigId      <- findSignatoryId sigInfo (eventInfoUser e) docId
                let link = mkLink baseUrl docId sigId
                pure $ InstanceUserAction userAction docId sigId link
              )
              userAllowedEvents
        in  pure $ mkGetInstanceView userActions InProgress
      _ -> inconsistentInstanceState instanceId user
  where
    findSignatoryId sigInfo name docId =
      snd3 <$> find (\(un, _, did) -> un == name && did == docId) sigInfo

    mkLink baseUrl documentId signatoryId =
      Url $ baseUrl <> showt (LinkSignDocNoMagicHash documentId signatoryId)

    toApiUserDocument
      :: [(UserName, SignatoryLinkID, DocumentID)]
      -> [EventInfo]
      -> Map DocumentName DocumentID
      -> DocumentName
      -> Maybe InstanceUserDocument
    toApiUserDocument sigInfo userReceivedEvents docMap docName = do
      docId <- Map.lookup docName docMap
      sigId <- findSignatoryId sigInfo userName docId
      pure $ InstanceUserDocument docId docState sigId
      where
        docEvents = filter (\e -> eventInfoDocument e == docName) userReceivedEvents
        docHasAction action =
          filter (\e -> eventInfoAction e == action) docEvents /= mempty
        docState | docHasAction Machinize.Approval = Approved
                 | docHasAction Machinize.Signature = Signed
                 | docHasAction Machinize.View = Viewed
                 | docHasAction Machinize.Rejection = Rejected
                 | otherwise                   = Started

checkInstancePerms :: Account -> InstanceId -> AccessAction -> AppM Model.Instance
checkInstancePerms account instanceId action = do
  flowInstance <- fromMaybeM throwInstanceNotFoundError $ Model.selectInstance instanceId
  let tid = flowInstance ^. #templateId
  template <- selectTemplate tid
  guardUserHasPermission account [canDo action . FlowTemplateR $ template ^. #folderId]
  pure flowInstance
