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
    , selectInstance
    , selectInstanceKeyValues
    )
  where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.State
import Data.Int (Int64)
import Data.Text (pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

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
fetchGetTemplate :: (TemplateId, Text, Text, Maybe UTCTime, Maybe UTCTime) -> GetTemplate
fetchGetTemplate (id, name, process, committed, deleted) =
  GetTemplate id name process committed deleted

-- TODO: Return maybe to indicate no template was found?
selectTemplate :: (MonadDB m, MonadThrow m) => TemplateId -> m GetTemplate
selectTemplate templateId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    sqlResult "id"
    sqlResult "name"
    sqlResult "process"
    sqlResult "committed"
    sqlResult "deleted"
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"
  fetchOne fetchGetTemplate

updateTemplate
  :: (MonadDB m, MonadThrow m) => TemplateId -> PatchTemplate -> m GetTemplate
updateTemplate templateId PatchTemplate {..} = do
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlMaybeSet "name"    name -- TODO: validate size?
    sqlMaybeSet "process" process -- TODO: validate size?
    sqlResult "id"
    sqlResult "name"
    sqlResult "process"
    sqlResult "committed"
    sqlResult "deleted"
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"
  fetchOne fetchGetTemplate

commitTemplate :: MonadDB m => UTCTime -> TemplateId -> m ()
commitTemplate now templateId = do
  runQuery_ . sqlUpdate "flow_templates" $ do
    sqlSet "committed" now -- TODO: make use of authenticated user
    sqlWhereEq "id" templateId
    sqlWhereIsNULL "deleted"

getTemplateDsl :: (MonadDB m, MonadThrow m) => TemplateId -> m FlowDSL
getTemplateDsl templateId = do
  runQuery_ . sqlSelect "flow_templates" $ do
    sqlResult "process"
    sqlWhereEq "id" templateId
  fetchOne runIdentity

insertParsedStateMachine :: MonadDB m => TemplateId -> Machine -> m ()
insertParsedStateMachine templateId machine = do
  runQuery_ . sqlInsert "flow_compiled_state_machine" $ do
    sqlSet "template_id" templateId
    sqlSet "data" . pack $ show machine

insertFlowInstance :: (MonadDB m, MonadThrow m) => TemplateId -> m InstanceId
insertFlowInstance templateId = do
  runQuery_ . sqlInsert "flow_instances" $ do
    sqlSet "template_id" templateId -- TODO: validate size?
    sqlResult "id"
  fetchOne runIdentity

insertFlowInstanceKeyValue
    -- TODO: Make Id type for original kontra IDs.
    -- TODO: Think of a way how to make this more typed. Text -> (text, Int64)
    -- is really ugly...
  :: MonadDB m => InstanceId -> Text -> (Text, Int64) -> m ()
insertFlowInstanceKeyValue instanceId valueType (key, value) =
  runQuery_ . sqlInsert "flow_instance_key_value_store" $ do
    sqlSet "instance_id" instanceId
    sqlSet "type"        valueType
    sqlSet "key"         key
    sqlSet "value"       value

-- TODO: Move to Maybe with selects?
selectInstance :: (MonadDB m, MonadThrow m) => InstanceId -> m TemplateId
selectInstance instanceId = do
  runQuery_ . sqlSelect "flow_instances" $ do
    sqlResult "template_id"
    sqlWhereEq "id" instanceId
  fetchOne runIdentity

-- TODO: Move to Maybe with selects?
selectInstanceKeyValues
  :: (MonadDB m, MonadThrow m) => InstanceId -> Text -> m [(Text, Int64)]
selectInstanceKeyValues instanceId valueType = do
  runQuery_ . sqlSelect "flow_instance_key_value_store" $ do
    sqlResult "key"
    sqlResult "value"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "type"        valueType
  fetchMany identity
