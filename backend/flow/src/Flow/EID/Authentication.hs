
module Flow.EID.Authentication
    ( selectFlowEidAuthentication
    , updateFlowEidAuthentication
    , insertAuthenticationFailure
    , checkAuthMaxFailuresExceeded
    )
  where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Time
import Data.Int

import DB
import Doc.Types.SignatoryLink
import EID.Authentication.Model
import Flow.Core.Type.AuthenticationConfiguration
import Flow.Id
import Flow.Names
import Session.SessionID

updateFlowEidAuthentication
  :: (MonadDB m, MonadMask m)
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> SessionID
  -> EAuthentication
  -> m ()
updateFlowEidAuthentication instanceId userName authKind sid eauth = do
  runQuery_ . sqlInsert "flow_eid_authentications" $ do
    setFields
    sqlOnConflictOnColumns ["instance_id", "user_name", "auth_kind"] . sqlUpdate "" $ do
      setFields
  where
    setFields :: (MonadState v n, SqlSet v) => n ()
    setFields = do
      sqlSet "session_id"  sid
      sqlSet "instance_id" instanceId
      sqlSet "user_name"   userName
      sqlSet "auth_kind"   authKind
      setEIDAuthentication eauth

selectFlowEidAuthentication
  :: (MonadThrow m, MonadDB m)
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> SessionID
  -> m (Maybe EAuthentication)
selectFlowEidAuthentication instanceId userName authKind sid = do
  runQuery_ . sqlSelect "flow_eid_authentications" $ do
    mapM_ sqlResult eidAuthenticationSelectors
    sqlWhereEq "session_id"  sid
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "user_name"   userName
    sqlWhereEq "auth_kind"   authKind
  fetchMaybe fetchEAuthentication

insertAuthenticationFailure
  :: (MonadThrow m, MonadDB m, MonadTime m)
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> m ()
insertAuthenticationFailure instanceId userName authKind = do
  now <- currentTime
  runQuery_ . sqlInsert "flow_eid_authentication_failures" $ do
    sqlSet "instance_id" instanceId
    sqlSet "user_name"   userName
    sqlSet "auth_kind"   authKind
    sqlSet "attempted"   now

countAuthenticationFailures
  :: (MonadThrow m, MonadDB m) => InstanceId -> UserName -> AuthenticationKind -> m Int64
countAuthenticationFailures instanceId userName authKind = do
  runQuery_ . sqlSelect "flow_eid_authentication_failures" $ do
    sqlResult "COUNT(*)"
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "user_name"   userName
    sqlWhereEq "auth_kind"   authKind
  fetchOne runIdentity

checkAuthMaxFailuresExceeded
  :: (MonadDB m, MonadThrow m)
  => InstanceId
  -> UserName
  -> (AuthenticationKind, AuthenticationConfiguration)
  -> m Bool
checkAuthMaxFailuresExceeded instanceId userName (authKind, authConfig) = do
  numFailures <- countAuthenticationFailures instanceId userName authKind
  pure $ fromIntegral numFailures > authConfig ^. #maxFailures
