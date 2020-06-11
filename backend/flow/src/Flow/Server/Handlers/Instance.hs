{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Flow.Server.Handlers.Instance where

import Control.Monad.Extra (fromMaybeM, whenM)
import Data.Aeson
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map)
import Data.Set (Set)
import Log.Class
import Servant
import qualified Data.Map as Map
import qualified Data.Set as Set

import AccessControl.Check
import AccessControl.Types
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import Flow.Aggregator as Aggregator
import Flow.Api as Api
import Flow.Error
import Flow.Guards
import Flow.HighTongue (DocumentName, UserName)
import Flow.Id
import Flow.Machinize as Machinize
import Flow.Model.Types as Model
import Flow.OrphanInstances ()
import Flow.Server.Types
import User.UserID (UserID, unsafeUserID)
import qualified Flow.Model as Model
import qualified Flow.Transducer as Transducer

startInstance :: Account -> TemplateId -> InstanceToTemplateMapping -> AppM StartTemplate
startInstance Account {..} templateId InstanceToTemplateMapping {..} = do
  logInfo_ "starting instance"
  -- TODO: Check permissions create instance..
  -- TODO: Check permissions to the template.
  -- TODO: Validate mapping...
  -- TODO: Check mapping value sizes???
  -- TODO: Replace value type with enum???
  -- TODO: Model instance state inside database somehow!
  whenM (isNothing <$> Model.selectTemplate templateId) throwTemplateNotFoundError
  id <- Model.insertFlowInstance templateId
  insertFlowInstanceKeyValues id documents StoreDocumentId
  insertFlowInstanceKeyValues id users     StoreUserId
  insertFlowInstanceKeyValues id messages  StoreMessage

  stateId <- Transducer.initialState <$> fromMaybeM
    throwTemplateNotCommittedError
    (Model.selectParsedStateMachine templateId)
  Model.insertAggregatorState (Aggregator.makeNewState stateId) id

  pure $ StartTemplate { id }
  where
    -- TODO: this probably needs to be moved to Model module.
    insertFlowInstanceKeyValues
      :: InstanceId -> Map.Map Text a -> (a -> StoreValue) -> AppM ()
    insertFlowInstanceKeyValues instanceId keyValues f =
      mapM_ (\(k, v) -> Model.insertFlowInstanceKeyValue instanceId k $ f v)
        $ Map.toList keyValues

getInstance :: Account -> InstanceId -> AppM GetInstance
getInstance account instanceId = do
  logInfo_ "getting instance"
  -- TODO: Authorize user.
  -- TODO: Model instance state inside database somehow!
  flowInstance <- checkInstancePerms account instanceId ReadA
  documents    <-
    Map.fromList
    .   fmap (fmap unsafeDocumentID)
    <$> Model.selectInstanceKeyValues instanceId Document
  users <-
    Map.fromList
    .   fmap (fmap unsafeUserID)
    <$> Model.selectInstanceKeyValues instanceId User
  messages <-
    Map.fromList
    .   fmap (fmap identity)
    <$> Model.selectInstanceKeyValues instanceId Message
  pure $ GetInstance { id                 = instanceId
                     , templateId         = templateId (flowInstance :: Model.Instance)
                     , templateParameters = InstanceToTemplateMapping { .. }
                     , state = InstanceState { availableActions = [], history = [] }
                     }

getInstanceView :: Account -> InstanceId -> AppM GetInstanceView
getInstanceView account@Account {..} instanceId = do
  logInfo "Getting instance view"
    $ object ["instance_id" .= instanceId, "account" .= account]

  -- TODO: Switch to using signatories
  -- TODO: Authorization

  documents <-
    Map.fromList
    .   fmap (fmap unsafeDocumentID)
    <$> Model.selectInstanceKeyValues instanceId Document
  users <-
    Map.fromList
    .   fmap (fmap unsafeUserID)
    <$> Model.selectInstanceKeyValues instanceId User

  (machine, aggrState) <- Model.selectAggregatorData instanceId

  case (getUserName (user ^. #id) users, mAllowedEvents machine aggrState) of
    (Just userName, Just allowedEvents) ->
      pure
        $ let
              -- Actions
              userAllowedEvents =
                Set.filter (\e -> eventInfoUser e == userName) allowedEvents
              userActions = catMaybes . Set.toList $ Set.map
                (\e -> do
                  deed  <- toApiDeed $ eventInfoDeed e
                  docId <- Map.lookup (eventInfoDocument e) documents
                  pure $ InstanceUserAction deed docId
                )
                userAllowedEvents

              -- Document state
              userReceivedEvents = Set.filter (\e -> eventInfoUser e == userName)
                $ Aggregator.receivedEvents (aggrState :: AggregatorState)
              userDocs          = Set.map eventInfoDocument userReceivedEvents
              userDocsWithState = catMaybes . Set.toList $ Set.map
                (toDocumentOverview userReceivedEvents documents)
                userDocs
          in  GetInstanceView
                { id      = instanceId
                , state   = InstanceUserState { documents = userDocsWithState }
                , actions = userActions
                }
    _ -> do
      logAttention "getInstanceView: Invalid userId or broken state for Flow instance "
        $ object ["instance_id" .= instanceId, "account" .= account]
      throwError
        $ err500 { errBody = "Could not reconstruct the state of the Flow process." }

  where
    mAllowedEvents machine aggregatorState =
      rightToMaybe $ Aggregator.getAllowedEvents <$> Transducer.getState
        machine
        (currentState aggregatorState)

    -- Assuming we allow only one UserName per user.
    getUserName :: UserID -> Map UserName UserID -> Maybe UserName
    getUserName uid userMap = case Map.keys $ Map.filter (== uid) userMap of
      []      -> Nothing
      (x : _) -> Just x

    toApiDeed :: Machinize.Deed -> Maybe Api.InstanceEventDeed
    toApiDeed Machinize.Approval  = Just Api.Approve
    toApiDeed Machinize.Signature = Just Api.Sign
    toApiDeed Machinize.View      = Just Api.View
    toApiDeed Machinize.Rejection = Just Api.Reject
    toApiDeed (Machinize.Field _) = Nothing
    toApiDeed Machinize.Timeout   = Nothing

    toDocumentOverview
      :: Set EventInfo
      -> Map DocumentName DocumentID
      -> DocumentName
      -> Maybe DocumentOverview
    toDocumentOverview userReceivedEvents docMap docName =
      (`DocumentOverview` docState) <$> Map.lookup docName docMap

      where
        docEvents = Set.filter (\e -> eventInfoDocument e == docName) userReceivedEvents
        docHasDeed deed =
          Set.filter (\e -> eventInfoDeed e == deed) docEvents /= Set.empty
        docState | docHasDeed Machinize.Approval = Approved
                 | docHasDeed Machinize.Signature = Signed
                 | docHasDeed Machinize.View     = Viewed
                 | docHasDeed Machinize.Rejection = Rejected
                 | otherwise                     = Started

checkInstancePerms :: Account -> InstanceId -> AccessAction -> AppM Instance
checkInstancePerms account instanceId action = do
  flowInstance <- fromMaybeM (throwError err404) $ Model.selectInstance instanceId
  let tid = templateId (flowInstance :: Instance)
  template <- fromMaybeM throwTemplateNotFoundError . Model.selectTemplate $ tid
  let fid = folderId (template :: GetTemplate)
  guardUserHasPermission account [canDo action $ FlowTemplateR fid]
  pure flowInstance
