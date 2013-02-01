module ELegitimation.ELegTransaction.Model (
    ELegTransaction(..)
  , MergeELegTransaction(..)
  , GetELegTransaction(..)
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Data
import Data.Monoid

import Context
import Crypto.RNG
import DB
import ELegitimation.BankIDRequests
import KontraMonad
import Doc.SignatoryLinkID
import Doc.DocumentID
import MagicHash
import Session.Model

-- | Wrapper for conversion between SQL and list of strings, as we don't want
-- to add instance for jsonizing [(String, String)] that is used only for that
-- specific purpose since it pollutes space of instances.
newtype CResAttributes = CResAttributes [(String, String)]
  deriving (Data, Typeable)
$(jsonableDeriveConvertible [t| CResAttributes |])
$(newtypeDeriveUnderlyingReadShow ''CResAttributes)

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
instance (CryptoRNG m, KontraMonad m, MonadDB m) => DBUpdate m MergeELegTransaction () where
  update (MergeELegTransaction ELegTransaction{..}) = do
    sid <- getNonTempSessionID
    kRun_ $ SQL "SELECT merge_eleg_transaction(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" [
        toSql transactiontransactionid
      , toSql sid
      , toSql transactionnonce
      , toSql transactiontbs
      , toSql transactionencodedtbs
      , toSql transactionsignatorylinkid
      , toSql transactiondocumentid
      , toSql transactionmagichash
      , toSql $ toStatus transactionstatus
      , toSql $ toCRTransactionID transactionstatus
      , toSql $ toCRSignature transactionstatus
      , toSql $ toCRAttributes transactionstatus
      , toSql transactionoref
      ]
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
    sid <- ctxsessionid `liftM` lift getContext
    _ <- kRun $ selectTransactionsSQL <> SQL "WHERE id = ? AND session_id = ?" [
        toSql tid
      , toSql sid
      ]
    fetchTransactions >>= oneObjectReturnedGuard

selectTransactionsSQL :: SQL
selectTransactionsSQL = "SELECT" <+> selectors <+> "FROM eleg_transactions "
  where
    selectors = sqlConcatComma [
        "id"
      , "nonce"
      , "tbs"
      , "encoded_tbs"
      , "signatory_link_id"
      , "document_id"
      , "token"
      , "status"
      , "cr_transaction_id"
      , "cr_signature"
      , "cr_attributes"
      , "oref"
      ]

fetchTransactions :: MonadDB m => DBEnv m [ELegTransaction]
fetchTransactions = kFold decoder []
  where
    decoder acc tid nonce tbs encoded_tbs slid did token status
      cr_transaction_id cr_signature cr_attributes oref = ELegTransaction {
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
        } : acc
    decodeTransactionStatus "_cr_outstanding" (Just tid, Nothing, Nothing) =
      Right $ CROutstanding { cresTransactionID = tid }
    decodeTransactionStatus "_cr_outstanding" d =
      error $ "Invalid data for CROutstanding: " ++ show d
    decodeTransactionStatus "_cr_user_sign" (Just tid, Nothing, Nothing) =
      Right $ CRUserSign { cresTransactionID = tid }
    decodeTransactionStatus "_cr_user_sign" d =
      error $ "Invalid data for CRUserSign: " ++ show d
    decodeTransactionStatus "_cr_complete" (Just tid, Just sig, Just (CResAttributes attrs)) =
      Right $ CRComplete { cresTransactionID = tid, cresSignature = sig, cresAttributes = attrs }
    decodeTransactionStatus "_cr_complete" d =
      error $ "Invalid data for CRComplete: " ++ show d
    decodeTransactionStatus status (Nothing, Nothing, Nothing) = Left status
    decodeTransactionStatus status d =
      error $ "Invalid data for status = " ++ status ++ ": " ++ show d
