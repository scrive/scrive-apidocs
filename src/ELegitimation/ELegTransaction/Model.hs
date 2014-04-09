module ELegitimation.ELegTransaction.Model (
    ELegTransaction(..)
  , MergeELegTransaction(..)
  , GetELegTransaction(..)
  ) where

import Control.Monad
import Control.Monad.State
import Data.Data

import Context
import Crypto.RNG
import DB
import ELegitimation.BankIDRequests
import KontraMonad
import Doc.SignatoryLinkID
import Doc.DocumentID
import MagicHash
import Session.Model
import Happstack.Server (ServerMonad)

-- | Wrapper for conversion between SQL and list of strings, as we don't want
-- to add instance for jsonizing [(String, String)] that is used only for that
-- specific purpose since it pollutes space of instances.
newtype CResAttributes = CResAttributes [(String, String)]
  deriving (Data, Typeable)
$(newtypeDeriveUnderlyingReadShow ''CResAttributes)

instance PQFormat CResAttributes where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL CResAttributes where
  type PQBase CResAttributes = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL CResAttributes where
  type PQDest CResAttributes = PQDest String
  toSQL = jsonToSQL

data ELegTransaction = ELegTransaction {
    transactiontransactionid   :: String
  , transactionnonce           :: Maybe String
  , transactiontbs             :: String
  , transactionencodedtbs      :: Maybe String
  , transactionsignatorylinkid :: Maybe SignatoryLinkID
  , transactiondocumentid      :: DocumentID
  , transactionmagichash       :: Maybe MagicHash
  , transactionstatus          :: (Either String CollectResponse)
  , transactionoref            :: Maybe String
  } deriving (Eq, Ord, Show, Typeable)

data MergeELegTransaction = MergeELegTransaction ELegTransaction
instance (ServerMonad m, CryptoRNG m, KontraMonad m, MonadDB m) => DBUpdate m MergeELegTransaction () where
  update (MergeELegTransaction ELegTransaction{..}) = do
    sid <- getNonTempSessionID
    runQuery_ $ rawSQL "SELECT merge_eleg_transaction($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)" (
        transactiontransactionid
      , sid
      , transactionnonce
      , transactiontbs
      , transactionencodedtbs
      , transactionsignatorylinkid
      , transactiondocumentid
      , transactionmagichash
      , toStatus transactionstatus
      , toCRTransactionID transactionstatus
      , toCRSignature transactionstatus
      , toCRAttributes transactionstatus
      , transactionoref
      )
    where
      toStatus (Left s) = s
      toStatus (Right CROutstanding{}) = "_cr_outstanding"
      toStatus (Right CRUserSign{}) = "_cr_user_sign"
      toStatus (Right CRComplete{}) = "_cr_complete"
      toCRTransactionID (Right cr) = Just $ cresTransactionID cr
      toCRTransactionID _ = Nothing
      toCRSignature (Right CRComplete{..}) = Just cresSignature
      toCRSignature _ = Nothing
      toCRAttributes (Right CRComplete{..}) = Just $ CResAttributes cresAttributes
      toCRAttributes _ = Nothing

data GetELegTransaction = GetELegTransaction String
instance (KontraMonad m, MonadDB m) => DBQuery m GetELegTransaction (Maybe ELegTransaction) where
  query (GetELegTransaction tid) = do
    sid <- ctxsessionid `liftM` getContext
    runQuery_ . selectTransactions $ do
      sqlWhereEq "id" tid
      sqlWhereEq "session_id" sid
    fetchMaybe fetchTransaction

selectTransactions :: State SqlSelect () -> SqlSelect
selectTransactions refine = sqlSelect "eleg_transactions" $ do
  sqlResult "id"
  sqlResult "nonce"
  sqlResult "tbs"
  sqlResult "encoded_tbs"
  sqlResult "signatory_link_id"
  sqlResult "document_id"
  sqlResult "token"
  sqlResult "status"
  sqlResult "cr_transaction_id"
  sqlResult "cr_signature"
  sqlResult "cr_attributes"
  sqlResult "oref"
  refine

fetchTransaction :: (String, Maybe String, String, Maybe String, Maybe SignatoryLinkID, DocumentID, Maybe MagicHash, String, Maybe String, Maybe String, Maybe CResAttributes, Maybe String) -> ELegTransaction
fetchTransaction (tid, nonce, tbs, encoded_tbs, slid, did, token, status, cr_transaction_id, cr_signature, cr_attributes, oref) = ELegTransaction {
  transactiontransactionid = tid
, transactionnonce = nonce
, transactiontbs = tbs
, transactionencodedtbs = encoded_tbs
, transactionsignatorylinkid = slid
, transactiondocumentid = did
, transactionmagichash = token
-- force evaluation to spot possible conversion error immediately
, transactionstatus = id $! decodeTransactionStatus status (cr_transaction_id, cr_signature, cr_attributes)
, transactionoref = oref
}
  where
    decodeTransactionStatus "_cr_outstanding" (Just tid', Nothing, Nothing) =
      Right $ CROutstanding { cresTransactionID = tid' }
    decodeTransactionStatus "_cr_outstanding" d =
      error $ "Invalid data for CROutstanding: " ++ show d
    decodeTransactionStatus "_cr_user_sign" (Just tid', Nothing, Nothing) =
      Right $ CRUserSign { cresTransactionID = tid' }
    decodeTransactionStatus "_cr_user_sign" d =
      error $ "Invalid data for CRUserSign: " ++ show d
    decodeTransactionStatus "_cr_complete" (Just tid', Just sig, Just (CResAttributes attrs)) =
      Right $ CRComplete { cresTransactionID = tid', cresSignature = sig, cresAttributes = attrs }
    decodeTransactionStatus "_cr_complete" d =
      error $ "Invalid data for CRComplete: " ++ show d
    decodeTransactionStatus status' (Nothing, Nothing, Nothing) = Left status'
    decodeTransactionStatus status' d =
      error $ "Invalid data for status = " ++ status' ++ ": " ++ show d
