{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Server.Api.Instances where

import Control.Monad.Except
import Control.Monad.Extra (fromMaybeM)
import Data.Aeson
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Log.Class
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import Auth.MagicHash
import Doc.DocumentID (DocumentID)
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
import Flow.Server.Types
import qualified Flow.Model as Model
import qualified Flow.Model.InstanceSession as Model
import qualified Flow.Model.Types as Model

accountEndpoints :: ServerT (AuthProtect "account" :> InstanceApi) AppM
accountEndpoints account = getInstance account :<|> listInstances account

-- TODO share with templates.start
mkAccessLinks :: Text -> InstanceId -> [(UserName, MagicHash)] -> Map UserName Text
mkAccessLinks baseUrl instanceId = Map.fromList . fmap (\(u, h) -> (u, mkAccessLink u h))
  where
    mkAccessLink userName hash = baseUrl <> "/flow/overview/" <> T.intercalate
      "/"
      [toUrlPiece instanceId, toUrlPiece userName, toUrlPiece hash]


getInstance :: Account -> InstanceId -> AppM GetInstance
getInstance account instanceId = do
  logInfo_ "Getting instance"
  flowInstance <- checkInstancePerms account instanceId ReadA
  keyValues    <- Model.selectInstanceKeyValues instanceId
  accessTokens <- Model.selectInstanceAccessTokens instanceId
  let usersWithHashes = fmap (\at -> (at ^. #userName, at ^. #hash)) accessTokens
  let access_links    = mkAccessLinks (baseUrl account) instanceId usersWithHashes
  pure $ GetInstance { id                 = instanceId
                     , templateId         = flowInstance ^. #templateId
                     , templateParameters = keyValues
                     -- TODO add a proper instance state
                     , state = InstanceState { availableActions = [], history = [] }
                     , access_links
                     }

listInstances :: Account -> AppM [GetInstance]
listInstances account@Account {..} = do
  is <- Model.selectInstancesByUserID $ user ^. #id
  let iids = map (view #id) is
  mapM (getInstance account) iids

getInstanceView :: InstanceUser -> InstanceId -> AppM GetInstanceView
getInstanceView user@InstanceUser {..} instanceId' = do
  logInfo "Getting instance view"
    $ object ["instance_id" .= instanceId', "instance_user" .= user]

  when (instanceId /= instanceId') $ throwAuthenticationError AccessControlError

  keyValues    <- Model.selectInstanceKeyValues instanceId
  fullInstance <- fromMaybeM (throwError err404) $ Model.selectFullInstance instanceId
  let aggrState = instanceToAggregator fullInstance
  machine <- decodeHighTongueM $ fullInstance ^. #template % #process

  case mAllowedEvents machine aggrState of
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
                  pure $ InstanceUserAction userAction docId
                )
                userAllowedEvents

              -- Document state
              userReceivedEvents = Set.filter (\e -> eventInfoUser e == userName)
                $ Aggregator.receivedEvents (aggrState :: AggregatorState)
              userDocs          = Set.map eventInfoDocument userReceivedEvents
              userDocsWithState = catMaybes . Set.toList $ Set.map
                (toDocumentOverview userReceivedEvents $ keyValues ^. #documents)
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
    mAllowedEvents HighTongue {..} AggregatorState {..} =
      fold
        .   Set.map (Set.fromList . expectToSuccess)
        .   stageExpect
        .   fst
        <$> remainingStages currentStage stages

    toApiUserAction :: Machinize.UserAction -> Maybe Api.InstanceEventAction
    toApiUserAction Machinize.Approval  = Just Api.Approve
    toApiUserAction Machinize.Signature = Just Api.Sign
    toApiUserAction Machinize.View      = Just Api.View
    toApiUserAction Machinize.Rejection = Just Api.Reject
    toApiUserAction (Machinize.Field _) = Nothing
    toApiUserAction Machinize.Timeout   = Nothing

    toDocumentOverview
      :: Set EventInfo
      -> Map DocumentName DocumentID
      -> DocumentName
      -> Maybe DocumentOverview
    toDocumentOverview userReceivedEvents docMap docName =
      (`DocumentOverview` docState) <$> Map.lookup docName docMap

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
