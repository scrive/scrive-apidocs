{-# LANGUAGE StrictData #-}

module Flow.Model.InstanceSession
    ( insertInstanceAccessToken
    , verifyInstanceAccessToken
    , upsertInstanceSession
    , selectInstanceSession
    )
  where

import Control.Monad.Catch (MonadThrow)
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

import Auth.MagicHash
import Auth.Session.SessionID
import Flow.Id
import Flow.Model.Types
import Flow.Names

insertInstanceAccessToken :: MonadDB m => InstanceId -> UserName -> MagicHash -> m ()
insertInstanceAccessToken instanceId userName hash =
  runQuery_ . sqlInsert "flow_instance_access_tokens" $ do
    sqlSet "instance_id" instanceId
    sqlSet "key"         userName
    sqlSet "hash"        hash

verifyInstanceAccessToken
  :: (MonadDB m, MonadThrow m)
  => InstanceId
  -> UserName
  -> MagicHash
  -> m (Maybe InstanceAccessTokenId)
verifyInstanceAccessToken instanceId userName hash = do
  runQuery_ . sqlSelect "flow_instance_access_tokens" $ do
    sqlResult "id"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "key"         userName
    sqlWhereEq "hash"        hash
    -- TODO: add expiry date
  fetchMaybe runIdentity

upsertInstanceSession :: MonadDB m => SessionID -> InstanceId -> UserName -> m ()
upsertInstanceSession sessionId instanceId userName =
  runQuery_ . sqlInsert "flow_instance_sessions" $ do
    sqlSet "session_id"  sessionId
    sqlSet "instance_id" instanceId
    sqlSet "key"         userName
    sqlOnConflictOnColumns ["session_id"] . sqlUpdate "" $ do
      sqlSet "instance_id" instanceId
      sqlSet "key"         userName

selectInstanceSession
  :: (MonadDB m, MonadThrow m) => SessionID -> m (Maybe InstanceSession)
selectInstanceSession sessionId = do
  runQuery_ . sqlSelect "flow_instance_sessions" $ do
    sqlResult "session_id"
    sqlResult "instance_id"
    sqlResult "key"
    sqlWhereEq "session_id" sessionId
  fetchMaybe fetchInstanceSession
