module EID.EIDService.Model (
    MergeEIDServiceTransaction(..)
  , GetEIDServiceTransaction(..)
  , PurgeTimeoutedEIDTransactions(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time

import DB
import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink
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
    update (MergeEIDServiceTransaction EIDServiceTransaction{..}) = do
      loopOnUniqueViolation . withSavepoint "merge_eid_service_transaction" $ do
        success <- runQuery01 . sqlUpdate "eid_service_transactions" $ do
          setFields
          sqlWhereEq "signatory_link_id" $ estSignatoryLinkID
          sqlWhereEq "auth_kind" $ estAuthKind
        unless success $
          runQuery_ . sqlInsert "eid_service_transactions" $ do
            setFields
      where
        setFields :: (MonadState v n, SqlSet v) => n ()
        setFields  = do
          sqlSet "transaction_id" $ estID
          sqlSet "status" $ estStatus
          sqlSet "signatory_link_id" $ estSignatoryLinkID
          sqlSet "auth_kind" $ estAuthKind
          sqlSet "session_id" $ estSessionID
          sqlSet "provider" $ estProvider
          sqlSet "deadline" $ estDeadline

data GetEIDServiceTransaction = GetEIDServiceTransaction SessionID SignatoryLinkID AuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransaction (Maybe EIDServiceTransaction) where
    query (GetEIDServiceTransaction sessionId slid authKind) = do
      runQuery_ . sqlSelect "eid_service_transactions" $ do
        mapM_ sqlResult selectEIDServiceTransaction
        sqlWhereEq "signatory_link_id" slid
        sqlWhereEq "auth_kind" $ authKind
        sqlWhereEq "session_id" sessionId
      fetchMaybe fetchEIDServiceTransaction

data PurgeTimeoutedEIDTransactions = PurgeTimeoutedEIDTransactions
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBUpdate m PurgeTimeoutedEIDTransactions () where
    update (PurgeTimeoutedEIDTransactions) = do
      ct <- currentTime
      runSQL_ $ "DELETE FROM eid_service_transactions WHERE deadline <" <?> ct

fetchEIDServiceTransaction :: (EIDServiceTransactionID, EIDServiceTransactionStatus, SignatoryLinkID, AuthenticationKind, SessionID, EIDServiceTransactionProvider, UTCTime) -> EIDServiceTransaction
fetchEIDServiceTransaction (estid, status, slid, auth_kind, session_id, provider, deadline) =
  EIDServiceTransaction
    { estID = estid
    , estStatus = status
    , estSignatoryLinkID = slid
    , estAuthKind = auth_kind
    , estProvider = provider
    , estSessionID = session_id
    , estDeadline = deadline
    }
