module Flow.Model
    ( deleteTemplate
    , insertTemplate
    , selectTemplate
    , updateTemplate
    , commitTemplate
    , getTemplateDsl
    , insertFlowInstance
    , insertFlowInstanceKeyValue
    , insertParsedStateMachine
    , selectParsedStateMachine
    , selectInstance
    , selectInstanceKeyValues
    , selectAggregatorState
    , insertAggregatorState
    , updateAggregatorState
    , selectAggregatorData
    , selectDocumentNameFromKV
    , selectUserNameFromKV
    )
  where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Aggregator
import Flow.Id
import Flow.Machinize
import Flow.Model.Types

-- TODO: Is it good idea to have Flow.Api used in the model???
-- MB: it isn't. But if we're going to create complex types
--     between the DB and the API, we should really reconsider
--     capabilities to properly separate the two.
import Flow.Api


sqlMaybeSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> Maybe a -> m ()
sqlMaybeSet sql = maybe (pure ()) (sqlSet sql)

insertTemplate :: (MonadIO m, MonadDB m, MonadThrow m) => InsertTemplate -> m TemplateId
insertTemplate InsertTemplate {..} = do
  runQuery_ . sqlInsert "flow_templates" $ do
    sqlSet "name"          name -- TODO: validate size?
    sqlSet "process"       process -- TODO: validate size?
    sqlSet "user_id"       userId
    sqlSet "user_group_id" userGroupId
    sqlResult "id"
  fetchOne runIdentity

deleteTemplate :: (MonadDB m, MonadIO m) => TemplateId -> m ()
deleteTemplate templateId = do
    -- TODO:  Can committed template be deleted? Maybe this should be in server instead?
  now <- liftIO getCurrentTime
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlSet "deleted" now
    sqlWhereEq "id" templateId

-- TODO: Maybe use uncurryN functions?
fetchGetTemplate :: (TemplateId, Text, Text, Maybe UTCTime) -> GetTemplate
fetchGetTemplate (id, name, process, committed) = GetTemplate id name process committed

selectTemplate :: (MonadDB m, MonadThrow m) => TemplateId -> m (Maybe GetTemplate)
selectTemplate templateId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    sqlResult "id"
    sqlResult "name"
    sqlResult "process"
    sqlResult "committed"
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"
  fetchMaybe fetchGetTemplate

updateTemplate
  :: (MonadDB m, MonadThrow m) => TemplateId -> PatchTemplate -> m (Maybe GetTemplate)
updateTemplate templateId PatchTemplate {..} = do
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlMaybeSet "name"    name -- TODO: validate size?
    sqlMaybeSet "process" process -- TODO: validate size?
    sqlResult "id"
    sqlResult "name"
    sqlResult "process"
    sqlResult "committed"
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"
  fetchMaybe fetchGetTemplate

commitTemplate :: MonadDB m => UTCTime -> TemplateId -> m ()
commitTemplate now templateId = do
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlSet "committed" now -- TODO: make use of authenticated user
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"

getTemplateDsl :: (MonadDB m, MonadThrow m) => TemplateId -> m (Maybe FlowDSL)
getTemplateDsl templateId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    sqlResult "process"
    sqlWhereEq "id" templateId
  fetchMaybe runIdentity

insertParsedStateMachine :: MonadDB m => TemplateId -> Machine -> m ()
insertParsedStateMachine templateId machine = do
  runQuery_ . sqlInsert "flow_compiled_state_machine" $ do
    sqlSet "template_id" templateId
    sqlSet "data"        machine

selectParsedStateMachine :: (MonadDB m, MonadThrow m) => TemplateId -> m Machine
selectParsedStateMachine templateId = do
  runQuery_ . sqlSelect "flow_compiled_state_machine" $ do
    sqlResult "data"
    sqlWhereEq "template_id" templateId
  fetchOne runIdentity

insertFlowInstance :: (MonadDB m, MonadThrow m) => TemplateId -> m InstanceId
insertFlowInstance templateId = do
  runQuery_ . sqlInsert "flow_instances" $ do
    sqlSet "template_id" templateId -- TODO: validate size?
    sqlResult "id"
  fetchOne runIdentity

insertFlowInstanceKeyValue
    -- TODO: Make Id type for original kontra IDs.
  :: MonadDB m => InstanceId -> Text -> StoreValue -> m ()
insertFlowInstanceKeyValue instanceId key value =
  runQuery_ . sqlInsert "flow_instance_key_value_store" $ do
    sqlSet "instance_id" instanceId
    sqlSet "key"         key
    case value of
      StoreDocumentId document -> do
        sqlSet "type" $ storeValueTypeToText Document
        sqlSet "document_id" document
      StoreUserId user -> do
        sqlSet "type" $ storeValueTypeToText User
        sqlSet "user_id" user
      StoreEmail email -> do
        sqlSet "type" $ storeValueTypeToText Email
        sqlSet "string" email
      StorePhoneNumber number -> do
        sqlSet "type" $ storeValueTypeToText PhoneNumber
        sqlSet "string" number
      StoreMessage msg -> do
        sqlSet "type" $ storeValueTypeToText Message
        sqlSet "string" msg

selectInstance :: (MonadDB m, MonadThrow m) => InstanceId -> m (Maybe Instance)
selectInstance instanceId = do
  runQuery_ . sqlSelect "flow_instances" $ do
    sqlResult "id"
    sqlResult "template_id"
    sqlWhereEq "id" instanceId
  fetchMaybe fetchInstance

fetchInstance :: (InstanceId, TemplateId) -> Instance
fetchInstance (id, templateId) = Instance { .. }

-- TODO: Think about making this function a bit more type safe???
selectInstanceKeyValues
  :: (MonadDB m, MonadThrow m, FromSQL t) => InstanceId -> StoreValueType -> m [(Text, t)]
selectInstanceKeyValues instanceId valueType = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "key"
    sqlResult $ storeValueTypeToValueColumn valueType
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "type" $ storeValueTypeToText valueType
  fetchMany identity
  where
    storeValueTypeToValueColumn Document    = "document_id"
    storeValueTypeToValueColumn User        = "user_id"
    storeValueTypeToValueColumn Email       = "string"
    storeValueTypeToValueColumn PhoneNumber = "string"
    storeValueTypeToValueColumn Message     = "string"

insertAggregatorState :: MonadDB m => AggregatorState -> InstanceId -> m ()
insertAggregatorState aggregatorState instanceId =
  runQuery_ . sqlInsert "flow_instance_aggregator" $ do
    sqlSet "instance_id" instanceId
    sqlSet "data"        aggregatorState

selectAggregatorState :: (MonadDB m, MonadThrow m) => InstanceId -> m AggregatorState
selectAggregatorState instanceId = do
  runQuery_ . sqlSelect "flow_instance_aggregator" $ do
    sqlResult "data"
    sqlWhereEq "instanceId" instanceId
  fetchOne runIdentity

updateAggregatorState :: (MonadDB m) => InstanceId -> AggregatorState -> m ()
updateAggregatorState instanceId aggregatorState = do
  runQuery_ . sqlUpdate "flow_instance_aggregator" $ do
    sqlSet "instance_id" instanceId
    sqlSet "data"        aggregatorState

selectAggregatorData
  :: (MonadDB m, MonadThrow m) => InstanceId -> m (Machine, AggregatorState)
selectAggregatorData instanceId = do
  runQuery_ . sqlSelect "flow_instances" $ do
    sqlResult "machine.data"
    sqlResult "aggregator.data"
    sqlJoinOn "flow_instance_aggregator aggregator"
              "flow_instances.id = aggregator.instance_id"
    sqlJoinOn "flow_compiled_state_machine machine"
              "flow_instances.template_id = machine.template_id"
    sqlWhereEq "flow_instances.id" instanceId
  fetchOne identity

selectDocumentNameFromKV
  :: (MonadDB m, MonadThrow m) => InstanceId -> DocumentID -> m (Maybe Text)
selectDocumentNameFromKV instanceId documentId = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "key"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "type" $ storeValueTypeToText Document
    sqlWhereEq "document_id" documentId
  fetchMaybe runIdentity

selectUserNameFromKV
  :: (MonadDB m, MonadThrow m) => InstanceId -> SignatoryLinkID -> m (Maybe Text)
selectUserNameFromKV instanceId signatoryLinkId = do
  runQuery_ . sqlSelect "flow_instance_signatories" $ do
    sqlResult "key"
    sqlWhereEq "instance_id"  instanceId
    sqlWhereEq "signatory_id" signatoryLinkId
  fetchMaybe runIdentity
