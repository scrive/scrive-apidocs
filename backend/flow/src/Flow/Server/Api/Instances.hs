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
import Text.StringTemplates.Templates (runTemplatesT)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.ActionConsumers
import Flow.Aggregator as Aggregator
import Flow.Core.Type.Url
import Flow.Engine
import Flow.Error
import Flow.Guards
import Flow.HighTongue
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.OrphanInstances ()
import Flow.Routes.Api as Api
import Flow.Routes.Types
import Flow.Server.Api.Common
import Flow.Server.Types
import Flow.Server.Utils
import KontraLink
import User.Lang
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.Model.Types as Model

accountEndpoints :: ServerT (AuthProtect "account" :> InstanceApi) AppM
accountEndpoints account = getInstance account :<|> listInstances account

-- brittany-disable-next-binding
userEndpoints
  :: InstanceUser
  -> (InstanceId -> Maybe Text -> IsSecure -> AppM GetInstanceView)
      :<|> (InstanceId -> RejectParam -> AppM NoContent)
userEndpoints user =
       getInstanceView user
  :<|> rejectInstance user

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
        getAllowedEvents expectToSuccess tongue aggrState
          >>= (mapM (toAuthorAction keyValues) . Set.toList)

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
    toAuthorAction :: InstanceKeyValues -> ExpectEvent -> Maybe InstanceAuthorAction
    toAuthorAction keyValues ExpectEvent { expectAction, expectUser, expectDocument } =
      do
        actionType     <- toApiUserAction expectAction
        actionUser     <- keyValues ^. #users % at expectUser
        doc            <- expectDocument
        actionDocument <- keyValues ^. #documents % at doc
        pure InstanceAuthorAction { actionType, actionUser, actionDocument }

    toTemplateParameters :: InstanceKeyValues -> [UserAuthConfig] -> TemplateParameters
    toTemplateParameters (InstanceKeyValues documents users messages) authConfigs =
      TemplateParameters documents (Map.mapWithKey toUserConfig users) messages
      where
        toUserConfig :: UserName -> FlowUserId -> UserConfig
        toUserConfig userName flowUserId =
          let uac = lookupUserAuthConfig userName authConfigs
          in  UserConfig { flowUserId
                         , authToView         = view #authToView =<< uac
                         , authToViewArchived = view #authToViewArchived =<< uac
                         }

        lookupUserAuthConfig :: UserName -> [UserAuthConfig] -> Maybe UserAuthConfig
        lookupUserAuthConfig userName = find
          (\uac -> uac ^. #userName == userName && uac ^. #instanceId == instanceId)

listInstances :: Account -> AppM [GetInstance]
listInstances account@Account { user } = do
  is <- Model.selectInstancesByUserID $ user ^. #id
  let iids = map (view #id) is
  mapM (getInstance account) iids


getAllowedEvents
  :: (Expect -> [ExpectEvent]) -> HighTongue -> AggregatorState -> Maybe (Set ExpectEvent)
getAllowedEvents expect HighTongue { stages } AggregatorState { currentStage, receivedEvents }
  = Set.filter (`Set.notMember` receivedEvents') <$> mEvents
  where
    receivedEvents' :: Set ExpectEvent
    receivedEvents' = Set.fromList $ fmap toExpectEvent receivedEvents

    mEvents :: Maybe (Set ExpectEvent)
    mEvents = fmap mapper mRestStages

    mapper :: (Stage, [Stage]) -> Set ExpectEvent
    mapper (stage, _) = fold . Set.map (Set.fromList . expect) $ stageExpect stage

    mRestStages :: Maybe (Stage, [Stage])
    mRestStages = remainingStages currentStage stages

getUserAllowedEvents
  :: (Expect -> [ExpectEvent])
  -> HighTongue
  -> AggregatorState
  -> UserName
  -> Maybe (Set ExpectEvent)
getUserAllowedEvents expect descriptor state username =
  Set.filter (\event -> expectUser event == username)
    <$> getAllowedEvents expect descriptor state

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
toApiUserAction Machinize.Approval          = Just Api.Approve
toApiUserAction Machinize.Signature         = Just Api.Sign
toApiUserAction Machinize.View              = Just Api.View
toApiUserAction Machinize.FlowRejection     = Just Api.Reject
toApiUserAction Machinize.DocumentRejection = Just Api.Reject
toApiUserAction (Machinize.Field _)         = Nothing
toApiUserAction Machinize.Timeout           = Nothing

-- To reject a flow instance, we currently short circuit the entire
-- machinize / aggregator / event processing mechanism, and
-- update the aggregator state and database directly.
-- This is because AppM does not support the necessary monad constraints
-- to call the process event functions..
rejectInstance :: InstanceUser -> InstanceId -> RejectParam -> AppM NoContent
rejectInstance InstanceUser { instanceId = userInstanceId, userName } targetInstanceId RejectParam { message = mRejectMessage }
  = do
    when (userInstanceId /= targetInstanceId)
      $ throwAuthenticationError AccessControlError

    logInfo "reject instance API called"
      $ object ["instance_id" .= targetInstanceId, "username" .= userName]

    fullInstance <-
      fromMaybeM throwInstanceNotFoundError . Model.selectFullInstance $ userInstanceId

    -- hlint messing with nested accessor
    let flowInstance = fullInstance ^. #flowInstance
        currentState = flowInstance ^. #currentState

    -- A flow instance cannot be rejected if it has been completed
    -- or has failed, i.e. rejected before.
    when (currentState == failureStageName || currentState == finalStageName)
      $ throwAuthenticationError RejectForbiddenError

    let mDetails :: Maybe EventDetails =
          fmap (RejectionEventDetails . RejectionDetails) mRejectMessage

    let eventInfo = EventInfo { eventInfoAction   = FlowRejection
                              , eventInfoUser     = userName
                              , eventInfoDocument = Nothing
                              , eventInfoDetails  = mDetails
                              }

    rejectAction <- toConsumableRejectAction targetInstanceId eventInfo mRejectMessage

    logInfo "inserting reject event"
      $ object ["instance_id" .= targetInstanceId, "event_info" .= eventInfo]

    -- FIXME: the handler should have called relevant functions like processFlowEvent,
    -- but we can't run those in AppM as of now.
    -- Create a FlowRejection event and insert it directly
    eventId <- insertEvent $ toInsertEvent targetInstanceId eventInfo

    logInfo "updating aggregator state to failure" $ object
      [ "instance_id" .= targetInstanceId
      , "event_id" .= eventId
      , "stage" .= failureStageName
      ]

    -- Update the aggregator state directly into a failure stage, since the flow is rejected
    updateAggregatorState targetInstanceId (makeNewState failureStageName) eventId True

    flowCtx <- ask
    void
      . runTemplatesT (T.unpack $ codeFromLang LANG_EN, templates flowCtx)
      $ consumeRejectionAction rejectAction

    return NoContent

getInstanceView
  :: InstanceUser -> InstanceId -> Maybe Host -> IsSecure -> AppM GetInstanceView
getInstanceView user@InstanceUser { instanceId = userInstanceId, userName } targetInstanceId mHost isSecure
  = do
    logInfo "Getting instance view"
      $ object ["instance_id" .= targetInstanceId, "instance_user" .= user]

    when (userInstanceId /= targetInstanceId)
      $ throwAuthenticationError AccessControlError

    flowCtx <- ask
    let baseUrl = mkBaseUrl (mainDomainUrl flowCtx) (isSecure == Secure) mHost

    keyValues    <- Model.selectInstanceKeyValues userInstanceId
    sigInfo      <- Model.selectSignatoryInfo userInstanceId
    fullInstance <-
      fromMaybeM throwInstanceNotFoundError . Model.selectFullInstance $ userInstanceId
    let aggrState = instanceToAggregator fullInstance
    tongue <- decodeHighTongueM $ fullInstance ^. #template % #process

    -- Document state
    -- TODO Flow: This works because there is at most one event per document
    -- for every user. We should get relevant document for given users and
    -- compute state of the document by folding over all events.
    let userReceivedEvents :: [EventInfo]
        userReceivedEvents =
          filter (\e -> eventInfoUser e == userName) . fmap toEventInfo $ allEvents
            fullInstance

        userDocs :: [DocumentName]
        userDocs = mapMaybe eventInfoDocument userReceivedEvents

        userDocsWithState :: [InstanceUserDocument]
        userDocsWithState = mapMaybe
          (toApiUserDocument sigInfo userReceivedEvents $ keyValues ^. #documents)
          userDocs

        flowInstance = fullInstance ^. #flowInstance

        mkGetInstanceView actions status = GetInstanceView
          { id        = targetInstanceId
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
      | otherwise -> do
        case getUserAllowedEvents expectToSuccess tongue aggrState userName of
          Just userAllowedEvents ->
            let mapper :: ExpectEvent -> Maybe InstanceUserAction
                mapper e = do
                  userAction <- toApiUserAction $ expectAction e
                  doc        <- expectDocument e
                  docId      <- Map.lookup doc (keyValues ^. #documents)
                  sigId      <- findSignatoryId sigInfo (expectUser e) docId
                  let link = mkLink baseUrl docId sigId
                  pure $ InstanceUserAction userAction docId sigId link

                userActions :: [InstanceUserAction]
                userActions = catMaybes . Set.toList $ Set.map mapper userAllowedEvents
            in  pure $ mkGetInstanceView userActions InProgress
          Nothing -> inconsistentInstanceState targetInstanceId user
  where
    findSignatoryId
      :: [(UserName, SignatoryLinkID, DocumentID)]
      -> UserName
      -> DocumentID
      -> Maybe SignatoryLinkID
    findSignatoryId sigInfo name docId =
      snd3 <$> find (\(un, _, did) -> un == name && did == docId) sigInfo

    mkLink :: Text -> DocumentID -> SignatoryLinkID -> Url
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
        docEvents = filter (\e -> eventInfoDocument e == Just docName) userReceivedEvents
        docHasAction action =
          filter (\e -> eventInfoAction e == action) docEvents /= mempty
        docState | docHasAction Machinize.Approval = Approved
                 | docHasAction Machinize.Signature = Signed
                 | docHasAction Machinize.View = Viewed
                 | docHasAction Machinize.FlowRejection = Rejected
                 | docHasAction Machinize.DocumentRejection = Rejected
                 | otherwise                   = Started

checkInstancePerms :: Account -> InstanceId -> AccessAction -> AppM Model.Instance
checkInstancePerms account instanceId action = do
  flowInstance <- fromMaybeM throwInstanceNotFoundError $ Model.selectInstance instanceId
  let tid = flowInstance ^. #templateId
  template <- selectTemplate tid
  guardUserHasPermission account [canDo action . FlowTemplateR $ template ^. #folderId]
  pure flowInstance
