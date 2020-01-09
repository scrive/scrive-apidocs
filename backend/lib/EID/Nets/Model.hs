module EID.Nets.Model (
    MergeNetsSignOrder(..)
  , GetNetsSignOrder(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State.Class
import Crypto.RNG (CryptoRNG)
import Data.Time

import DB
import Doc.SignatoryLinkID
import EID.Nets.SignID
import EID.Nets.Types
import Session.SessionID

selectNetsSignOrderSelectorsList :: [SQL]
selectNetsSignOrderSelectorsList =
  [ "signatory_link_id"
  , "session_id"
  , "provider"
  , "text_to_be_signed"
  , "order_id"
  , "deadline"
  , "is_canceled"
  , "ssn"
  ]

-- | Insert new transaction or replace the existing one.
data MergeNetsSignOrder = MergeNetsSignOrder NetsSignOrder
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeNetsSignOrder () where
  update (MergeNetsSignOrder NetsSignOrder {..}) = do
    runQuery_ . sqlInsert "nets_sign_orders" $ do
      setFields
      sqlOnConflictOnColumns ["signatory_link_id"] . sqlUpdate "" $ do
        setFields
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" $ nsoSignatoryLinkID
        sqlSet "session_id" $ nsoSessionID
        sqlSet "provider" $ nsoProvider
        sqlSet "text_to_be_signed" $ nsoTextToBeSigned
        sqlSet "order_id" $ nsoSignOrderID
        sqlSet "deadline" $ nsoDeadline
        sqlSet "is_canceled" $ nsoIsCanceled
        sqlSet "ssn" $ nsoSSN

data GetNetsSignOrder = GetNetsSignOrder SignatoryLinkID
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetNetsSignOrder (Maybe NetsSignOrder) where
  query (GetNetsSignOrder slid) = do
    runQuery_ . sqlSelect "nets_sign_orders" $ do
      mapM_ sqlResult selectNetsSignOrderSelectorsList
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchNetsSignOrder

fetchNetsSignOrder
  :: ( SignatoryLinkID
     , SessionID
     , NetsSignProvider
     , Text
     , SignOrderUUID
     , UTCTime
     , Bool
     , Maybe Text
     )
  -> NetsSignOrder
fetchNetsSignOrder (slid, session_id, provider, ttbs, oid, deadline, is_canceled, mSSN) =
  NetsSignOrder { nsoSignOrderID     = oid
                , nsoSignatoryLinkID = slid
                , nsoProvider        = provider
                , nsoTextToBeSigned  = ttbs
                , nsoSessionID       = session_id
                , nsoDeadline        = deadline
                , nsoIsCanceled      = is_canceled
                , nsoSSN             = mSSN
                }
