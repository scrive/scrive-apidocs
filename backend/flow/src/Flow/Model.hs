module Flow.Model
    ( deleteTemplate
    , insertTemplate
    , selectMaybeTemplate
    , selectTemplatesByUserID
    , updateTemplate
    , insertFlowInstance
    , updateInstanceLastModified
    , insertEvent
    , insertFlowInstanceKeyValues
    , insertInstanceSignatories
    , selectInstanceEvents
    , selectDocumentIdsByDocumentIds
    , selectDocumentIdsAssociatedWithSomeInstance
    , selectDocumentNameFromKV
    , selectDocumentIdsByInstanceId
    , selectFullInstance
    , selectInstance
    , selectInstanceIdByDocumentId
    , selectInstanceKeyValues
    , selectInstancesByUserID
    , selectSignatoryIdsByInstanceUser
    , selectSignatoryInfo
    , selectUserNameFromKV
    , updateAggregatorState
    )
  where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State
import Control.Monad.Time
import Data.Tuple.Extra
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder
import qualified Data.Map as Map

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Aggregator
import Flow.Id
import Flow.Message
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Names
import User.UserID (UserID)

sqlMaybeSet :: (MonadState v m, SqlSet v, Show a, ToSQL a) => SQL -> Maybe a -> m ()
sqlMaybeSet sql = maybe (pure ()) (sqlSet sql)

insertTemplate :: (MonadDB m, MonadTime m, MonadThrow m) => InsertTemplate -> m TemplateId
insertTemplate it = do
  now <- currentTime
  runQuery_ . sqlInsert "flow_templates" $ do
    sqlSet "name" $ it ^. #name -- TODO: validate size?
    sqlSet "process" $ it ^. #process -- TODO: validate size?
    sqlSet "user_id" $ it ^. #userId
    sqlSet "folder_id" $ it ^. #folderId
    sqlSet "created" now
    sqlResult "id"
  fetchOne runIdentity

deleteTemplate :: (MonadDB m, MonadTime m) => TemplateId -> m ()
deleteTemplate templateId = do
  now <- currentTime
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

selectMaybeTemplate :: (MonadDB m, MonadThrow m) => TemplateId -> m (Maybe Template)
selectMaybeTemplate templateId = do
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

insertFlowInstance :: (MonadDB m, MonadThrow m) => InsertInstance -> m InstanceId
insertFlowInstance InsertInstance {..} = do
  runQuery_ . sqlInsert "flow_instances" $ do
    sqlSet "template_id"   templateId
    sqlSet "current_state" stage
    sqlSet "started"       started
    sqlSet "last_event"    started
    whenJust title $ sqlSet "title"
    sqlResult "id"
  fetchOne runIdentity

insertFlowInstanceKeyValues :: MonadDB m => InstanceId -> InstanceKeyValues -> m ()
insertFlowInstanceKeyValues instanceId keyValues =
  unless (null keys) . runQuery_ . sqlInsert "flow_instance_key_value_store" $ do
    sqlSet "instance_id" instanceId
    sqlSetList "key"  keys
    sqlSetList "type" types
    sqlSetListWithDefaults "document_id" documentIds
    sqlSetListWithDefaults "user_id"     userIds
    sqlSetListWithDefaults "string"      strings
  where
    documents = keyValues ^. #documents
    users     = keyValues ^. #users
    messages  = keyValues ^. #messages
    textKeys :: Map.Map (Name a) b -> [Text]
    textKeys = fmap fromName . Map.keys
    keys :: [Text]
    keys           = textKeys documents <> textKeys users <> textKeys messages
    documentValues = Map.elems documents
    userValues     = Map.elems users
    messageValues  = Map.elems messages
    applyToValues
      :: (Show a, ToSQL a)
      => (DocumentID -> a)
      -> (FlowUserId -> a)
      -> (Message -> a)
      -> [a]
    applyToValues f g h =
      fmap f documentValues <> fmap g userValues <> fmap h messageValues
    types :: [Text]
    types = applyToValues (const "document") (fst3 . toUserInfo) (const "message")

    toUserInfo :: FlowUserId -> (Text, Maybe UserID, Maybe Text)
    toUserInfo = \case
      Email       e   -> ("email", Nothing, Just e)
      PhoneNumber pn  -> ("phone_number", Nothing, Just pn)
      UserId      uid -> ("user", Just uid, Nothing)

    documentIds :: [Maybe DocumentID]
    documentIds = applyToValues Just (const Nothing) (const Nothing)

    userIds :: [Maybe UserID]
    userIds = applyToValues (const Nothing) (snd3 . toUserInfo) (const Nothing)

    strings :: [Maybe Text]
    strings = applyToValues (const Nothing) (thd3 . toUserInfo) (Just . fromMessage)

selectDocumentIdsByDocumentIds :: MonadDB m => [DocumentID] -> m [DocumentID]
selectDocumentIdsByDocumentIds docIds = do
  runQuery_ . sqlSelect "documents d" $ do
    sqlResult "d.id"
    sqlWhereIn "d.id" docIds
  fetchMany runIdentity

selectDocumentIdsAssociatedWithSomeInstance :: MonadDB m => [DocumentID] -> m [DocumentID]
selectDocumentIdsAssociatedWithSomeInstance docIds = do
  runQuery_ . sqlSelect "flow_instance_key_value_store i" $ do
    sqlResult "i.document_id"
    sqlWhereIn "i.document_id" docIds
  fetchMany runIdentity

instanceSelectors :: (MonadState v m, SqlResult v) => SQL -> m ()
instanceSelectors prefix = mapM_
  (\c -> sqlResult $ prefix <> "." <> c)
  ["id", "template_id", "title", "current_state", "started", "last_event"]

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

-- Copy-paste from flow-document-events branch
selectInstanceIdByDocumentId
  :: (MonadDB m, MonadThrow m) => DocumentID -> m (Maybe InstanceId)
selectInstanceIdByDocumentId documentId = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "instance_id"
    sqlWhereEq "document_id" documentId
    sqlWhereEq "type"        ("document" :: Text)
  fetchMaybe runIdentity

fetchKeyValues
  :: MonadDB m => m [(Text, Text, Maybe DocumentID, Maybe UserID, Maybe Text)]
fetchKeyValues = fetchMany identity

selectInstanceKeyValues :: (MonadDB m, MonadThrow m) => InstanceId -> m InstanceKeyValues
selectInstanceKeyValues instanceId = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "key"
    sqlResult "type"
    sqlResult "document_id"
    sqlResult "user_id"
    sqlResult "string"
    sqlWhereEq "instance_id" instanceId
  toInstanceKeyValues <$> fetchKeyValues
  where
    convert
      :: (KeyValueTuple -> Maybe (Name a, b)) -> [KeyValueTuple] -> Map.Map (Name a) b
    convert f = Map.fromList . mapMaybe f

    toInstanceKeyValues :: [KeyValueTuple] -> InstanceKeyValues
    toInstanceKeyValues keyValues = InstanceKeyValues documents users messages
      where
        documents = convert toDocument keyValues
        users     = convert toUser keyValues
        messages  = convert toMessage keyValues

    toDocument :: KeyValueTuple -> Maybe (DocumentName, DocumentID)
    toDocument = \case
      (k, "document", Just did, _, _) -> Just (unsafeName k, did)
      _ -> Nothing

    toUser :: KeyValueTuple -> Maybe (UserName, FlowUserId)
    toUser = \case
      (k, "email", _, _, Just e) -> Just (unsafeName k, Email e)
      (k, "phone_number", _, _, Just pn) -> Just (unsafeName k, PhoneNumber pn)
      (k, "user", _, Just uid, _) -> Just (unsafeName k, UserId uid)
      _ -> Nothing

    toMessage :: KeyValueTuple -> Maybe (MessageName, Message)
    toMessage = \case
      (k, "message", _, _, Just s) -> Just (unsafeName k, unsafeMessage s)
      _ -> Nothing

type KeyValueTuple = (Text, Text, Maybe DocumentID, Maybe UserID, Maybe Text)

-- TODO write this as a single query
selectFullInstance :: (MonadDB m, MonadThrow m) => InstanceId -> m (Maybe FullInstance)
selectFullInstance id = do
  maybeInstance <- selectInstance id
  case maybeInstance of
    Nothing           -> pure Nothing
    Just flowInstance -> do
      -- This is guaranteed by a foreign key.
      template         <- fromJust <$> selectMaybeTemplate (flowInstance ^. #templateId)
      aggregatorEvents <- selectInstanceEvents id True
      allEvents        <- selectInstanceEvents id False
      pure $ Just FullInstance { .. }

updateAggregatorState
  :: (MonadDB m) => InstanceId -> AggregatorState -> EventId -> Bool -> m ()
updateAggregatorState instanceId AggregatorState {..} eventId stateChange = do
  runQuery_ . sqlUpdate "flow_instances" $ do
    sqlSet "current_state" currentStage
    sqlWhereEq "id" instanceId

  if stateChange
    then runQuery_ . sqlDelete "flow_aggregator_events" $ do
      sqlFrom "flow_events"
      sqlWhere "flow_aggregator_events.id = flow_events.id"
      sqlWhereEq "flow_events.instance_id" instanceId
    else runQuery_ . sqlInsert "flow_aggregator_events" $ do
      sqlSet "id" eventId

updateInstanceLastModified :: (MonadDB m, MonadTime m, MonadThrow m) => InstanceId -> m ()
updateInstanceLastModified instanceId = do
  now <- currentTime
  runQuery_ . sqlUpdate "flow_instances" $ do
    sqlSet "last_event" now
    sqlWhereEq "id" instanceId

insertEvent :: (MonadDB m, MonadTime m, MonadThrow m) => InsertEvent -> m EventId
insertEvent ie = do
  now <- currentTime
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

selectInstanceEvents :: (MonadDB m, MonadThrow m) => InstanceId -> Bool -> m [Event]
selectInstanceEvents instanceId onlyAggregatorEvents = do
  runQuery_ . sqlSelect "flow_instances i" $ do
    eventSelectors "e"
    sqlJoinOn "flow_events e" "e.instance_id = i.id"
    when onlyAggregatorEvents $ sqlJoinOn "flow_aggregator_events ae" "ae.id = e.id"
    sqlWhereEq "i.id" instanceId
    sqlOrderBy "e.created DESC"
  fetchMany fetchEvent

selectDocumentNameFromKV
  :: (MonadDB m, MonadThrow m) => InstanceId -> DocumentID -> m (Maybe DocumentName)
selectDocumentNameFromKV instanceId documentId = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "key"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "type"        ("document" :: Text)
    sqlWhereEq "document_id" documentId
  fetchMaybe runIdentity

insertInstanceSignatories
  :: MonadDB m => InstanceId -> [(UserName, SignatoryLinkID)] -> m ()
insertInstanceSignatories instanceId links =
  unless (null links) . runQuery_ . sqlInsert "flow_instance_signatories" $ do
    sqlSet "instance_id" instanceId
    sqlSetList "key" $ map fst links
    sqlSetList "signatory_id" $ map snd links

selectDocumentIdsByInstanceId :: (MonadDB m, MonadThrow m) => InstanceId -> m [DocumentID]
selectDocumentIdsByInstanceId instanceId = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "document_id"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "type"        ("document" :: Text)
  fetchMany runIdentity

selectUserNameFromKV
  :: (MonadDB m, MonadThrow m) => InstanceId -> SignatoryLinkID -> m (Maybe UserName)
selectUserNameFromKV instanceId signatoryLinkId = do
  runQuery_ . sqlSelect "flow_instance_signatories" $ do
    sqlResult "key"
    sqlWhereEq "instance_id"  instanceId
    sqlWhereEq "signatory_id" signatoryLinkId
  fetchMaybe runIdentity

selectSignatoryIdsByInstanceUser
  :: (MonadDB m, MonadThrow m) => InstanceId -> UserName -> m [SignatoryLinkID]
selectSignatoryIdsByInstanceUser instanceId userName = do
  runQuery_ . sqlSelect "flow_instance_signatories" $ do
    sqlResult "signatory_id"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "key"         userName
  fetchMany runIdentity

selectSignatoryInfo
  :: (MonadDB m, MonadThrow m)
  => InstanceId
  -> m [(UserName, SignatoryLinkID, DocumentID)]
selectSignatoryInfo instanceId = do
  runQuery_ . sqlSelect "flow_instance_signatories fis" $ do
    sqlJoinOn "signatory_links sl" "sl.id = fis.signatory_id"
    sqlResult "fis.key"
    sqlResult "fis.signatory_id"
    sqlResult "sl.document_id"
    sqlWhereEq "fis.instance_id" instanceId
  fetchMany identity
