module EID.CGI.GRP.Transaction.Model (
    CgiGrpTransaction(..)
  , MergeCgiGrpTransaction(..)
  , GetCgiGrpTransaction(..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Text (Text)
import Happstack.Server

import Context
import Crypto.RNG
import DB
import Doc.SignatoryLinkID
import KontraMonad
import Session.Model

data CgiGrpTransaction = CgiGrpTransaction {
  cgtSignatoryLinkID :: !SignatoryLinkID
, ctgTextToBeSigned  :: !Text
, cgtTransactionID   :: !Text
, cgtOrderRef        :: !Text
} deriving (Eq, Ord, Show)

----------------------------------------

-- | Insert new transaction or replace the existing one if exists.
data MergeCgiGrpTransaction = MergeCgiGrpTransaction CgiGrpTransaction
instance (CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, ServerMonad m)
  => DBUpdate m MergeCgiGrpTransaction () where
    update (MergeCgiGrpTransaction CgiGrpTransaction{..}) = do
      sid <- getNonTempSessionID
      runQuery_ $ rawSQL "SELECT merge_cgi_grp_transaction($1, $2, $3, $4, $5)"
        (cgtSignatoryLinkID, sid, ctgTextToBeSigned, cgtTransactionID, cgtOrderRef)

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
