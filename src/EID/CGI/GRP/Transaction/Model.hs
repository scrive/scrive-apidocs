module EID.CGI.GRP.Transaction.Model (
    CgiGrpTransaction(..)
  , MergeCgiGrpTransaction(..)
  , GetCgiGrpTransaction(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.Text (Text)
import Happstack.Server

import Context
import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad
import Session.Model
import Session.SessionID

data CgiGrpTransaction = CgiGrpTransaction {
  cgtSignatoryLinkID :: !SignatoryLinkID
, ctgTextToBeSigned  :: !Text
, cgtTransactionID   :: !Text
, cgtOrderRef        :: !Text
} deriving (Eq, Ord, Show)

----------------------------------------

-- | Insert new transaction or replace the existing one.
data MergeCgiGrpTransaction = MergeCgiGrpTransaction CgiGrpTransaction
instance (CryptoRNG m, KontraMonad m, MonadDB m, MonadMask m, ServerMonad m)
  => DBUpdate m MergeCgiGrpTransaction () where
    update (MergeCgiGrpTransaction CgiGrpTransaction{..}) = do
      sid <- getNonTempSessionID
      loopOnUniqueViolation . withSavepoint "merge_cgi_grp_transaction" $ do
        success <- runQuery01 . sqlUpdate "cgi_grp_transactions" $ do
          setFields sid
          sqlWhereEq "signatory_link_id" cgtSignatoryLinkID
        when (not success) $ do
          runQuery_ . sqlInsert "cgi_grp_transactions" $ do
            setFields sid
      where
        setFields :: (MonadState v n, SqlSet v) => SessionID -> n ()
        setFields sid = do
          sqlSet "signatory_link_id" cgtSignatoryLinkID
          sqlSet "session_id" sid
          sqlSet "text_to_be_signed" ctgTextToBeSigned
          sqlSet "transaction_id" cgtTransactionID
          sqlSet "order_ref" cgtOrderRef

data GetCgiGrpTransaction = GetCgiGrpTransaction SignatoryLinkID
instance (KontraMonad m, MonadDB m, MonadThrow m)
  => DBQuery m GetCgiGrpTransaction (Maybe CgiGrpTransaction) where
    query (GetCgiGrpTransaction slid) = do
      sid <- ctxsessionid <$> getContext
      runQuery_ . sqlSelect "cgi_grp_transactions" $ do
        sqlResult "signatory_link_id"
        sqlResult "text_to_be_signed"
        sqlResult "transaction_id"
        sqlResult "order_ref"
        sqlWhereEq "signatory_link_id" slid
        sqlWhereEq "session_id" sid
      fetchMaybe fetchCgiGrpTransaction

----------------------------------------

fetchCgiGrpTransaction :: (SignatoryLinkID, Text, Text, Text) -> CgiGrpTransaction
fetchCgiGrpTransaction (slid, ttbs, transaction_id, order_ref) = CgiGrpTransaction {
  cgtSignatoryLinkID = slid
, ctgTextToBeSigned = ttbs
, cgtTransactionID = transaction_id
, cgtOrderRef = order_ref
}
