{-# LANGUAGE Strict #-}

module Flow.Model
    ( deleteTemplate
    , insertTemplate
    , selectTemplate
    , selectTemplatesByUserID
    , updateTemplate
    , insertFlowInstance
    , insertFlowInstanceKeyValue
    , selectInstance
    , selectInstancesByUserID
    , selectInstanceKeyValues
    , selectFullInstance
    , selectAggregatorEvents
    , updateAggregatorState
    , insertEvent
    , selectDocumentNameFromKV
    , selectUserNameFromKV
    )
  where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Aggregator
import Flow.Id
import Flow.Model.Types
import User.UserID (UserID)

sqlMaybeSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> Maybe a -> m ()
sqlMaybeSet sql = maybe (pure ()) (sqlSet sql)

insertTemplate :: (MonadIO m, MonadDB m, MonadThrow m) => InsertTemplate -> m TemplateId
insertTemplate it = do
  now <- liftIO getCurrentTime
  runQuery_ . sqlInsert "flow_templates" $ do
    sqlSet "name" $ it ^. #name -- TODO: validate size?
    sqlSet "process" $ it ^. #process -- TODO: validate size?
    sqlSet "user_id" $ it ^. #userId
    sqlSet "folder_id" $ it ^. #folderId
    sqlSet "created" now
    sqlResult "id"
  fetchOne runIdentity

deleteTemplate :: (MonadDB m, MonadIO m) => TemplateId -> m ()
deleteTemplate templateId = do
  -- TODO:  Can committed template be deleted? Maybe this should be in server instead?
  now <- liftIO getCurrentTime
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlSet "deleted" now
    sqlWhereEq "id" templateId


templateSelectors :: (MonadState v m, SqlResult v) => m ()
templateSelectors = do
  sqlResult "id"
  sqlResult "user_id"
  sqlResult "folder_id"
  sqlResult "name"
  sqlResult "process"
  sqlResult "created"
  sqlResult "committed"
  sqlResult "deleted"

selectTemplate :: (MonadDB m, MonadThrow m) => TemplateId -> m (Maybe Template)
selectTemplate templateId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    templateSelectors
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"
  fetchMaybe fetchTemplate

selectTemplatesByUserID :: (MonadDB m, MonadThrow m) => UserID -> m [Template]
selectTemplatesByUserID userId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    templateSelectors
    sqlWhereEq "user_id" userId
    sqlWhereIsNULL "deleted"
  fetchMany fetchTemplate

updateTemplate :: (MonadDB m, MonadThrow m) => UpdateTemplate -> m (Maybe Template)
updateTemplate ut = do
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlMaybeSet "name" $ ut ^. #name
    sqlMaybeSet "process" $ ut ^. #process
    sqlMaybeSet "committed" $ ut ^. #committed
    templateSelectors
    sqlWhereEq "id" $ ut ^. #id
    sqlWhereIsNULL "deleted"
  fetchMaybe fetchTemplate

insertFlowInstance
  :: (MonadDB m, MonadIO m, MonadThrow m) => InsertInstance -> m InstanceId
insertFlowInstance ii = do
  now <- liftIO getCurrentTime
  runQuery_ . sqlInsert "flow_instances" $ do
    sqlSet "template_id" $ ii ^. #templateId
    sqlSet "current_state" $ ii ^. #currentState
    sqlSet "created" now
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

instanceSelectors :: (MonadState v m, SqlResult v) => SQL -> m ()
instanceSelectors prefix = mapM_ (\c -> sqlResult $ prefix <> "." <> c)
                                 ["id", "template_id", "current_state", "created"]

selectInstance :: (MonadDB m, MonadThrow m) => InstanceId -> m (Maybe Instance)
selectInstance instanceId = do
  runQuery_ . sqlSelect "flow_instances i" $ do
    instanceSelectors "i"
    sqlWhereEq "id" instanceId
  fetchMaybe fetchInstance

selectInstancesByUserID :: (MonadDB m, MonadThrow m) => UserID -> m [Instance]
selectInstancesByUserID userId = do
  runQuery_ . sqlSelect "flow_instances i" $ do
    instanceSelectors "i"
    sqlJoinOn "flow_templates t" "template_id = t.id"
    sqlWhereEq "t.user_id" userId
    sqlWhereIsNULL "t.deleted"
  fetchMany fetchInstance

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

-- TODO write this as a single query
selectFullInstance :: (MonadDB m, MonadThrow m) => InstanceId -> m (Maybe FullInstance)
selectFullInstance id = do
  maybeInstance <- selectInstance id
  case maybeInstance of
    Nothing           -> pure Nothing
    Just flowInstance -> do
      -- This is guaranteed by a foreign key.
      template         <- fromJust <$> selectTemplate (flowInstance ^. #templateId)
      aggregatorEvents <- selectAggregatorEvents id
      pure $ Just FullInstance { .. }

updateAggregatorState
  :: (MonadDB m) => InstanceId -> AggregatorState -> EventId -> Bool -> m ()
updateAggregatorState instanceId AggregatorState {..} eventId stateChange = do
  runQuery_ . sqlUpdate "flow_instances" $ do
    sqlSet "current_state" currentState
    sqlWhereEq "instance_id" instanceId

  if stateChange
    then runQuery_ . sqlDelete "flow_aggregator_events ae" $ do
      sqlFrom "flow_events ae"
      sqlWhereEq "ae.instance_id" instanceId
    else runQuery_ . sqlInsert "flow_aggregator_events" $ do
      sqlSet "id" eventId

insertEvent :: (MonadDB m, MonadIO m, MonadThrow m) => InsertEvent -> m EventId
insertEvent ie = do
  now <- liftIO getCurrentTime
  runQuery_ . sqlInsert "flow_events" $ do
    sqlSet "instance_id" $ ie ^. #instanceId
    sqlSet "user_name" $ ie ^. #userName
    sqlSet "document_name" $ ie ^. #documentName
    sqlSet "user_action" $ ie ^. #userAction
    sqlSet "created" now
    sqlResult "id"
  fetchOne runIdentity

eventSelectors :: (MonadState v m, SqlResult v) => SQL -> m ()
eventSelectors prefix = do
  mapM_ (\c -> sqlResult $ prefix <> "." <> c)
        ["id", "instance_id", "user_name", "document_name", "user_action", "created"]

selectAggregatorEvents :: (MonadDB m, MonadThrow m) => InstanceId -> m [Event]
selectAggregatorEvents instanceId = do
  runQuery_ . sqlSelect "flow_instances i" $ do
    eventSelectors "e"
    sqlJoinOn "flow_events e"             "e.instance_id = i.id"
    sqlJoinOn "flow_aggregator_events ae" "ae.id = e.id"
    sqlWhereEq "i.id" instanceId
    sqlOrderBy "e.created DESC"
  fetchMany fetchEvent

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
