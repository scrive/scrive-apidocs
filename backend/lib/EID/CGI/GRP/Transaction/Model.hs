module EID.CGI.GRP.Transaction.Model (
    CgiGrpTransactionType(..)
  , CgiGrpTransaction(..)
  , cgiSignatoryLinkID
  , cgiTransactionID
  , cgiOrderRef
  , cgiTransactionType
  , MergeCgiGrpTransaction(..)
  , GetCgiGrpTransaction(..)
  , DeleteCgiGrpTransaction(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Crypto.RNG
import Data.Data
import Data.Int

import DB
import Doc.SignatoryLinkID
import Session.SessionID

data CgiGrpTransactionType = CgiGrpAuth | CgiGrpSign
  deriving (Eq, Ord, Show, Typeable)

instance PQFormat CgiGrpTransactionType where
  pqFormat = pqFormat @Int16

instance FromSQL CgiGrpTransactionType where
  type PQBase CgiGrpTransactionType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return CgiGrpAuth
      2  -> return CgiGrpSign
      _  -> throwM RangeError {
        reRange = [(1,2)]
      , reValue = n
      }

instance ToSQL CgiGrpTransactionType where
  type PQDest CgiGrpTransactionType = PQDest Int16
  toSQL CgiGrpAuth = toSQL (1::Int16)
  toSQL CgiGrpSign = toSQL (2::Int16)

data CgiGrpTransaction =
    CgiGrpAuthTransaction !SignatoryLinkID  !Text !Text !SessionID
  | CgiGrpSignTransaction !SignatoryLinkID  !Text !Text !Text !SessionID
  deriving (Eq, Ord, Show)


cgiTransactionType :: CgiGrpTransaction -> CgiGrpTransactionType
cgiTransactionType (CgiGrpAuthTransaction _ _ _ _) = CgiGrpAuth
cgiTransactionType (CgiGrpSignTransaction _ _ _ _ _) = CgiGrpSign

cgiSignatoryLinkID :: CgiGrpTransaction -> SignatoryLinkID
cgiSignatoryLinkID (CgiGrpAuthTransaction slid _ _ _) = slid
cgiSignatoryLinkID (CgiGrpSignTransaction slid _ _ _ _) = slid

cgiTextToBeSigned :: CgiGrpTransaction -> Maybe Text
cgiTextToBeSigned (CgiGrpAuthTransaction _ _ _ _) = Nothing
cgiTextToBeSigned (CgiGrpSignTransaction _ tbs _ _ _) = Just tbs

cgiTransactionID  :: CgiGrpTransaction -> Text
cgiTransactionID  (CgiGrpAuthTransaction _ tid _ _) = tid
cgiTransactionID  (CgiGrpSignTransaction _ _ tid _ _) = tid

cgiOrderRef :: CgiGrpTransaction -> Text
cgiOrderRef (CgiGrpAuthTransaction _ _ orf _) = orf
cgiOrderRef (CgiGrpSignTransaction _ _ _ orf _) = orf

cgiSessionID :: CgiGrpTransaction -> SessionID
cgiSessionID (CgiGrpAuthTransaction _ _ _ session_id) = session_id
cgiSessionID (CgiGrpSignTransaction _ _ _ _ session_id) = session_id

----------------------------------------

-- | Insert new transaction or replace the existing one.
data MergeCgiGrpTransaction = MergeCgiGrpTransaction CgiGrpTransaction
instance (CryptoRNG m, MonadDB m, MonadMask m)
  => DBUpdate m MergeCgiGrpTransaction () where
    update (MergeCgiGrpTransaction cgiTransaction) = do
      loopOnUniqueViolation . withSavepoint "merge_cgi_grp_transaction" $ do
        success <- runQuery01 . sqlUpdate "cgi_grp_transactions" $ do
          setFields
          sqlWhereEq "signatory_link_id" $ cgiSignatoryLinkID cgiTransaction
          sqlWhereEq "type" $ cgiTransactionType cgiTransaction
        unless success $
          runQuery_ . sqlInsert "cgi_grp_transactions" $ do
            setFields
      where
        setFields :: (MonadState v n, SqlSet v) => n ()
        setFields  = do
          sqlSet "type" $ cgiTransactionType cgiTransaction
          sqlSet "signatory_link_id" $ cgiSignatoryLinkID cgiTransaction
          sqlSet "session_id" $ cgiSessionID cgiTransaction
          sqlSet "text_to_be_signed" $ cgiTextToBeSigned cgiTransaction
          sqlSet "transaction_id" $ cgiTransactionID cgiTransaction
          sqlSet "order_ref" $ cgiOrderRef cgiTransaction

data DeleteCgiGrpTransaction = DeleteCgiGrpTransaction CgiGrpTransactionType SignatoryLinkID
instance (MonadDB m, MonadMask m) => DBUpdate m DeleteCgiGrpTransaction () where
    update (DeleteCgiGrpTransaction cgitt slid) = do
       runQuery_ . sqlDelete "cgi_grp_transactions" $ do
          sqlWhereEq "signatory_link_id" $ slid
          sqlWhereEq "type" $ cgitt


data GetCgiGrpTransaction = GetCgiGrpTransaction CgiGrpTransactionType SignatoryLinkID
instance (MonadDB m, MonadThrow m)
  => DBQuery m GetCgiGrpTransaction (Maybe CgiGrpTransaction) where
    query (GetCgiGrpTransaction cgitt slid) = do
      runQuery_ . sqlSelect "cgi_grp_transactions" $ do
        sqlResult "type"
        sqlResult "signatory_link_id"
        sqlResult "text_to_be_signed"
        sqlResult "transaction_id"
        sqlResult "order_ref"
        sqlResult "session_id"
        sqlWhereEq "signatory_link_id" slid
        sqlWhereEq "type" cgitt
      fetchMaybe fetchCgiGrpTransaction

----------------------------------------

fetchCgiGrpTransaction :: (CgiGrpTransactionType,SignatoryLinkID, Maybe Text, Text, Text, SessionID) -> CgiGrpTransaction
fetchCgiGrpTransaction (cgitt, slid, mttbs, transaction_id, order_ref, session_id) =
    case cgitt of
      CgiGrpAuth -> CgiGrpAuthTransaction  slid transaction_id order_ref session_id
      CgiGrpSign -> CgiGrpSignTransaction slid (fromMaybe (unexpectedError "CGI Transaction field has NULL as text_to_be_signed") mttbs) transaction_id order_ref session_id
