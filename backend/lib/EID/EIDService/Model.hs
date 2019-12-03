module EID.EIDService.Model (
    MergeEIDServiceTransaction(..)
  , GetEIDServiceTransactionNoSessionIDGuard(..)
  , GetEIDServiceTransactionGuardSessionID(..)
  , PurgeTimeoutedEIDTransactions(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time

import DB
import Doc.SignatoryLinkID
import EID.EIDService.Types
import MinutesTime
import Session.SessionID

selectEIDServiceTransaction :: [SQL]
selectEIDServiceTransaction =
  [ "transaction_id"
  , "status"
  , "signatory_link_id"
  , "auth_kind"
  , "session_id"
  , "provider"
  , "deadline"
  ]

-- | Insert new transaction or replace the existing one.
data MergeEIDServiceTransaction = MergeEIDServiceTransaction EIDServiceTransaction
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeEIDServiceTransaction () where
  update (MergeEIDServiceTransaction EIDServiceTransaction {..}) = do
    runQuery_ . sqlInsert "eid_service_transactions" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id", "auth_kind"] . sqlUpdate "" $ do
        setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "transaction_id" $ estID
        sqlSet "status" $ estStatus
        sqlSet "signatory_link_id" $ estSignatoryLinkID
        sqlSet "auth_kind" $ estAuthKind
        sqlSet "session_id" $ estSessionID
        sqlSet "provider" $ estProvider
        sqlSet "deadline" $ estDeadline

data GetEIDServiceTransactionNoSessionIDGuard = GetEIDServiceTransactionNoSessionIDGuard SignatoryLinkID EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionNoSessionIDGuard (Maybe EIDServiceTransaction) where
  query (GetEIDServiceTransactionNoSessionIDGuard slid eidAuthKind) =
    getEIDServiceTransactionInternal Nothing slid eidAuthKind

data GetEIDServiceTransactionGuardSessionID = GetEIDServiceTransactionGuardSessionID SessionID SignatoryLinkID EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionGuardSessionID (Maybe EIDServiceTransaction) where
  query (GetEIDServiceTransactionGuardSessionID sessionId slid eidAuthKind) = do
    getEIDServiceTransactionInternal (Just sessionId) slid eidAuthKind

getEIDServiceTransactionInternal
  :: (MonadDB m, MonadThrow m)
  => (Maybe SessionID)
  -> SignatoryLinkID
  -> EIDServiceAuthenticationKind
  -> m (Maybe EIDServiceTransaction)
getEIDServiceTransactionInternal mSessionId slid eidAuthKind = do
  runQuery_ . sqlSelect "eid_service_transactions" $ do
    mapM_ sqlResult selectEIDServiceTransaction
    sqlWhereEq "signatory_link_id" slid
    sqlWhereEq "auth_kind" $ eidAuthKind
    -- When the sessionId is included, we check that it matches. This is used in
    -- authToView for example. Cron signing consumer does not have the sessionId, but
    -- it still needs the EID transaction status.
    whenJust mSessionId $ sqlWhereEq "session_id"
  fetchMaybe fetchEIDServiceTransaction

data PurgeTimeoutedEIDTransactions = PurgeTimeoutedEIDTransactions
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBUpdate m PurgeTimeoutedEIDTransactions () where
  update (PurgeTimeoutedEIDTransactions) = do
    ct <- currentTime
    runSQL_ $ "DELETE FROM eid_service_transactions WHERE deadline <" <?> ct

fetchEIDServiceTransaction
  :: ( EIDServiceTransactionID
     , EIDServiceTransactionStatus
     , SignatoryLinkID
     , EIDServiceAuthenticationKind
     , SessionID
     , EIDServiceTransactionProvider
     , UTCTime
     )
  -> EIDServiceTransaction
fetchEIDServiceTransaction (estid, status, slid, auth_kind, session_id, provider, deadline)
  = EIDServiceTransaction { estID              = estid
                          , estStatus          = status
                          , estSignatoryLinkID = slid
                          , estAuthKind        = auth_kind
                          , estProvider        = provider
                          , estSessionID       = session_id
                          , estDeadline        = deadline
                          }
