module EID.EIDService.Model (
    MergeEIDServiceTransaction(..)
  , GetEIDServiceTransactionNoSessionIDGuard(..)
  , GetEIDServiceTransactionGuardSessionID(..)
  , PurgeTimeoutedEIDTransactions(..)
  , eidServiceConf
  ) where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time

import DB
import Doc.SignatoryLinkID
import Doc.Types.Document
import EID.EIDService.Conf
import EID.EIDService.Types
import Kontra
import MinutesTime
import Session.SessionID
import UserGroup.Model
import UserGroup.Types
import Util.SignatoryLinkUtils

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
newtype MergeEIDServiceTransaction = MergeEIDServiceTransaction EIDServiceTransactionFromDB
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeEIDServiceTransaction () where
  dbUpdate (MergeEIDServiceTransaction EIDServiceTransactionFromDB {..}) = do
    runQuery_ . sqlInsert "eid_service_transactions" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id", "auth_kind"] . sqlUpdate "" $ do
        setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "transaction_id"    estID
        sqlSet "status"            estStatus
        sqlSet "signatory_link_id" estSignatoryLinkID
        sqlSet "auth_kind"         estAuthKind
        sqlSet "session_id"        estSessionID
        sqlSet "provider"          estProvider
        sqlSet "deadline"          estDeadline

data GetEIDServiceTransactionNoSessionIDGuard = GetEIDServiceTransactionNoSessionIDGuard SignatoryLinkID EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionNoSessionIDGuard (Maybe EIDServiceTransactionFromDB) where
  dbQuery (GetEIDServiceTransactionNoSessionIDGuard slid eidAuthKind) =
    getEIDServiceTransactionInternal Nothing slid eidAuthKind

data GetEIDServiceTransactionGuardSessionID = GetEIDServiceTransactionGuardSessionID SessionID SignatoryLinkID EIDServiceAuthenticationKind
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetEIDServiceTransactionGuardSessionID (Maybe EIDServiceTransactionFromDB) where
  dbQuery (GetEIDServiceTransactionGuardSessionID sessionId slid eidAuthKind) = do
    getEIDServiceTransactionInternal (Just sessionId) slid eidAuthKind

getEIDServiceTransactionInternal
  :: (MonadDB m, MonadThrow m)
  => Maybe SessionID
  -> SignatoryLinkID
  -> EIDServiceAuthenticationKind
  -> m (Maybe EIDServiceTransactionFromDB)
getEIDServiceTransactionInternal mSessionId slid eidAuthKind = do
  runQuery_ . sqlSelect "eid_service_transactions" $ do
    mapM_ sqlResult selectEIDServiceTransaction
    sqlWhereEq "signatory_link_id" slid
    sqlWhereEq "auth_kind"         eidAuthKind
    -- When the sessionId is included, we check that it matches. This is used in
    -- authToView for example. Cron signing consumer does not have the sessionId, but
    -- it still needs the EID transaction status.
    whenJust mSessionId $ sqlWhereEq "session_id"
  fetchMaybe fetchEIDServiceTransaction

data PurgeTimeoutedEIDTransactions = PurgeTimeoutedEIDTransactions
instance (MonadDB m, MonadThrow m, MonadTime m)
  => DBUpdate m PurgeTimeoutedEIDTransactions Int where
  dbUpdate PurgeTimeoutedEIDTransactions = do
    ct <- currentTime
    runSQL $ "DELETE FROM eid_service_transactions WHERE deadline <" <?> ct

fetchEIDServiceTransaction
  :: ( EIDServiceTransactionID
     , EIDServiceTransactionStatus
     , SignatoryLinkID
     , EIDServiceAuthenticationKind
     , SessionID
     , EIDServiceTransactionProvider
     , UTCTime
     )
  -> EIDServiceTransactionFromDB
fetchEIDServiceTransaction (estid, status, slid, auth_kind, session_id, provider, deadline)
  = EIDServiceTransactionFromDB { estID              = estid
                                , estStatus          = status
                                , estSignatoryLinkID = slid
                                , estAuthKind        = auth_kind
                                , estProvider        = provider
                                , estSessionID       = session_id
                                , estDeadline        = deadline
                                }

eidServiceConf :: Kontrakcja m => Document -> m EIDServiceConf
eidServiceConf doc = do
  ctx <- getContext
  case ctx ^. #eidServiceConf of
    Nothing    -> noConfigurationError "No eid service provided"
    Just conf0 -> do
      let err =
            unexpectedError $ "Impossible happened - no author for document: " <> showt
              (documentid doc)
      authorid <- maybe err return $ getAuthorUserId doc
      ugwp     <- dbQuery . UserGroupGetWithParentsByUserID $ authorid
      return $ case ugwpSettings ugwp ^. #eidServiceToken of
        Nothing    -> conf0
        Just token -> set #eidServiceToken token conf0
