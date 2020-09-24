module Flow.EID.EIDService.Model (
    MergeEIDServiceTransaction(..)
  , GetEIDServiceTransactionGuardSessionID(..)
  , GetEIDServiceTransactionNoSessionIDGuard(..)
  , getAnyDocumentWithSl
  ) where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time

import DB
import Doc.DocumentID
import Doc.Model.Query
import Doc.Types.SignatoryLink
import EID.EIDService.Types
import Flow.Id
import Flow.Model
import Flow.Names
import Session.SessionID
import qualified Flow.EID.EIDService.Types as FEET

-- | Insert new transaction or replace the existing one.
newtype MergeEIDServiceTransaction = MergeEIDServiceTransaction FEET.EIDServiceTransactionFromDB
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeEIDServiceTransaction () where
  dbUpdate (MergeEIDServiceTransaction FEET.EIDServiceTransactionFromDB {..}) = do
    runQuery_ . sqlInsert "flow_eid_service_transactions" $ do
      setFields
      sqlOnConflictOnColumns ["instance_id", "user_name", "auth_kind"] . sqlUpdate "" $ do
        setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "transaction_id" estID
        sqlSet "status"         estStatus
        sqlSet "instance_id"    estInstanceId
        sqlSet "user_name"      estUserName
        sqlSet "auth_kind"      estAuthKind
        sqlSet "session_id"     estSessionID
        sqlSet "provider"       estProvider
        sqlSet "deadline"       estDeadline

data GetEIDServiceTransactionNoSessionIDGuard = GetEIDServiceTransactionNoSessionIDGuard InstanceId UserName EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionNoSessionIDGuard (Maybe FEET.EIDServiceTransactionFromDB) where
  dbQuery (GetEIDServiceTransactionNoSessionIDGuard instanceId userName eidAuthKind) = do
    getEIDServiceTransactionInternal Nothing instanceId userName eidAuthKind

data GetEIDServiceTransactionGuardSessionID = GetEIDServiceTransactionGuardSessionID SessionID InstanceId UserName EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionGuardSessionID (Maybe FEET.EIDServiceTransactionFromDB) where
  dbQuery (GetEIDServiceTransactionGuardSessionID sessionId instanceId userName eidAuthKind)
    = do
      getEIDServiceTransactionInternal (Just sessionId) instanceId userName eidAuthKind

getEIDServiceTransactionInternal
  :: (MonadDB m, MonadThrow m)
  => Maybe SessionID
  -> InstanceId
  -> UserName
  -> EIDServiceAuthenticationKind
  -> m (Maybe FEET.EIDServiceTransactionFromDB)
getEIDServiceTransactionInternal mSessionId instanceId userName eidAuthKind = do
  runQuery_ . sqlSelect "flow_eid_service_transactions" $ do
    mapM_ sqlResult selectEIDServiceTransaction
    sqlWhereEq "instance_id" instanceId
    sqlWhereEq "user_name"   userName
    sqlWhereEq "auth_kind"   eidAuthKind
    -- When the sessionId is included, we check that it matches. This is used in
    -- authToView for example. Cron signing consumer does not have the sessionId, but
    -- it still needs the EID transaction status.
    whenJust mSessionId $ sqlWhereEq "session_id"
  fetchMaybe fetchEIDServiceTransaction

selectEIDServiceTransaction :: [SQL]
selectEIDServiceTransaction =
  [ "instance_id"
  , "user_name"
  , "auth_kind"
  , "session_id"
  , "transaction_id"
  , "status"
  , "provider"
  , "deadline"
  ]

fetchEIDServiceTransaction
  :: ( InstanceId
     , UserName
     , EIDServiceAuthenticationKind
     , SessionID
     , EIDServiceTransactionID
     , EIDServiceTransactionStatus
     , EIDServiceTransactionProvider
     , UTCTime
     )
  -> FEET.EIDServiceTransactionFromDB
fetchEIDServiceTransaction (estInstanceId, estUserName, estAuthKind, estSessionID, estID, estStatus, estProvider, estDeadline)
  = FEET.EIDServiceTransactionFromDB { .. }

getAnyDocumentWithSl
  :: (MonadDB m, MonadThrow m) => InstanceId -> UserName -> m (SignatoryLink, DocumentID)
getAnyDocumentWithSl instanceId userName = do
  -- TODO: Temporary - refactor into function when FLOW-325 is merged
  signatoryInfo <- find (\(userName', _, _) -> userName' == userName)
    <$> selectSignatoryInfo instanceId
  case signatoryInfo of
    Just (_, slid, did) -> fmap (, did) <$> dbQuery $ GetSignatoryLinkByID did slid
    Nothing -> unexpectedError "getAnyDocumentWithS1: signatory info not found!"
