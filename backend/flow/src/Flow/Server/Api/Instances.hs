{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Api.Instances where

import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Reader
import Data.Aeson
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple.Extra
import Log.Class
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set

import AccessControl.Check
import AccessControl.Types
import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Aggregator as Aggregator
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
  accessTokens <- Model.selectInstanceAccessTokens instanceId
  let usersWithHashes = fmap (\at -> (at ^. #userName, at ^. #hash)) accessTokens
  let accessLinks     = mkAccessLinks (baseUrl account) instanceId usersWithHashes
  pure $ GetInstance { id                 = instanceId
                     , templateId         = flowInstance ^. #templateId
                     , templateParameters = keyValues
                     -- TODO add a proper instance state
                     , state = InstanceState { availableActions = [], history = [] }
                     , accessLinks
                     }

listInstances :: Account -> AppM [GetInstance]
listInstances account@Account {..} = do
  is <- Model.selectInstancesByUserID $ user ^. #id
  let iids = map (view #id) is
  mapM (getInstance account) iids

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
  fullInstance <- fromMaybeM (throwError err404) $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  tongue <- decodeHighTongueM $ fullInstance ^. #template % #process

  case mAllowedEvents tongue aggrState of
    Just allowedEvents ->
      pure
        $ let
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

              -- Document state
              userReceivedEvents = Set.filter (\e -> eventInfoUser e == userName)
                $ Aggregator.receivedEvents (aggrState :: AggregatorState)
              userDocs          = Set.map eventInfoDocument userReceivedEvents
              userDocsWithState = catMaybes . Set.toList $ Set.map
                (toApiUserDocument sigInfo userReceivedEvents $ keyValues ^. #documents)
                userDocs
          in  GetInstanceView
                { id      = instanceId
                , state   = InstanceUserState { Api.documents = userDocsWithState }
                , actions = userActions
                }
    _ -> do
      logAttention "GetInstanceView: Invalid userId or broken state for Flow instance "
        $ object ["instance_id" .= instanceId, "instance_user" .= user]
      throwInternalServerError "Could not reconstruct the state of the Flow process."
  where
    findSignatoryId sigInfo name docId =
      snd3 <$> find (\(un, _, did) -> un == name && did == docId) sigInfo

    mAllowedEvents HighTongue {..} aggrState@AggregatorState {..} =
      Set.filter (`Set.notMember` Aggregator.receivedEvents aggrState)
        .   fold
        .   Set.map (Set.fromList . expectToSuccess)
        .   stageExpect
        .   fst
        <$> remainingStages currentStage stages

    mkLink baseUrl documentId signatoryId =
      Url $ baseUrl <> showt (LinkSignDocNoMagicHash documentId signatoryId)

    toApiUserAction :: Machinize.UserAction -> Maybe Api.InstanceEventAction
    toApiUserAction Machinize.Approval  = Just Api.Approve
    toApiUserAction Machinize.Signature = Just Api.Sign
    toApiUserAction Machinize.View      = Just Api.View
    toApiUserAction Machinize.Rejection = Just Api.Reject
    toApiUserAction (Machinize.Field _) = Nothing
    toApiUserAction Machinize.Timeout   = Nothing

    toApiUserDocument
      :: [(UserName, SignatoryLinkID, DocumentID)]
      -> Set EventInfo
      -> Map DocumentName DocumentID
      -> DocumentName
      -> Maybe InstanceUserDocument
    toApiUserDocument sigInfo userReceivedEvents docMap docName = do
      docId <- Map.lookup docName docMap
      sigId <- findSignatoryId sigInfo userName docId
      pure $ InstanceUserDocument docId docState sigId

      where
        docEvents = Set.filter (\e -> eventInfoDocument e == docName) userReceivedEvents
        docHasAction action =
          Set.filter (\e -> eventInfoAction e == action) docEvents /= Set.empty
        docState | docHasAction Machinize.Approval = Approved
                 | docHasAction Machinize.Signature = Signed
                 | docHasAction Machinize.View = Viewed
                 | docHasAction Machinize.Rejection = Rejected
                 | otherwise                   = Started

checkInstancePerms :: Account -> InstanceId -> AccessAction -> AppM Model.Instance
checkInstancePerms account instanceId action = do
  flowInstance <- fromMaybeM (throwError err404) $ Model.selectInstance instanceId
  let tid = flowInstance ^. #templateId
  template <- fromMaybeM throwTemplateNotFoundError $ Model.selectTemplate tid
  guardUserHasPermission account [canDo action . FlowTemplateR $ template ^. #folderId]
  pure flowInstance
