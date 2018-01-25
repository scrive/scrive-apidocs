module EID.Nets.Model (
    MergeNetsSignOrder(..)
  , GetNetsSignOrder(..)
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time
import qualified Data.Text as T

import DB
import Doc.SignatoryLinkID
import EID.Nets.Data
import EID.Nets.SignID
import KontraPrelude
import Session.SessionID

selectNetsSignOrderSelectorsList :: [SQL]
selectNetsSignOrderSelectorsList =
  [ "signatory_link_id"
  , "session_id"
  , "text_to_be_signed"
  , "order_id"
  , "ssn"
  , "deadline"
  , "is_canceled"
  ]

-- | Insert new transaction or replace the existing one.
data MergeNetsSignOrder = MergeNetsSignOrder NetsSignOrder
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeNetsSignOrder () where
    update (MergeNetsSignOrder NetsSignOrder{..}) = do
      loopOnUniqueViolation . withSavepoint "merge_nets_transaction" $ do
        success <- runQuery01 . sqlUpdate "nets_sign_orders" $ do
          setFields
          sqlWhereEq "signatory_link_id" $ nsoSignatoryLinkID
        when (not success) $ do
          runQuery_ . sqlInsert "nets_sign_orders" $ do
            setFields
      where
        setFields :: (MonadState v n, SqlSet v) => n ()
        setFields  = do
          sqlSet "signatory_link_id" $ nsoSignatoryLinkID
          sqlSet "session_id" $ nsoSessionID
          sqlSet "text_to_be_signed" $ nsoTextToBeSigned
          sqlSet "order_id" $ nsoSignOrderID
          sqlSet "ssn" $ nsoSignatorySSN
          sqlSet "deadline" $ nsoDeadline
          sqlSet "is_canceled" $ nsoIsCanceled

data GetNetsSignOrder = GetNetsSignOrder SignatoryLinkID
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetNetsSignOrder (Maybe NetsSignOrder) where
    query (GetNetsSignOrder slid) = do
      runQuery_ . sqlSelect "nets_sign_orders" $ do
        mapM_ sqlResult selectNetsSignOrderSelectorsList
        sqlWhereEq "signatory_link_id" slid
      fetchMaybe fetchNetsSignOrder

fetchNetsSignOrder :: (SignatoryLinkID, SessionID, T.Text, SignOrderUUID, T.Text, UTCTime, Bool) -> NetsSignOrder
fetchNetsSignOrder (slid, session_id, ttbs, oid, ssn, deadline, is_canceled) =
  NetsSignOrder
    { nsoSignOrderID = oid
    , nsoSignatoryLinkID = slid
    , nsoTextToBeSigned = ttbs
    , nsoSessionID = session_id
    , nsoSignatorySSN = ssn
    , nsoDeadline = deadline
    , nsoIsCanceled = is_canceled
    }
