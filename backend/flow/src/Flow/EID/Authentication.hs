
module Flow.EID.Authentication
    ( selectFlowEidAuthentication
    , updateFlowEidAuthentication
    )
  where

import Control.Monad.Catch
import Control.Monad.State

import DB
import Doc.Types.SignatoryLink
import EID.Authentication.Model
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
