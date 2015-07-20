module EID.Authentication.Model (
    EAuthentication(..)
  -- from EID.CGI.GRP.Data
  , BankIDAuthentication(..)
  , MergeBankIDAuthentication(..)
  , GetEAuthentication(..)
  , GetEAuthenticationWithoutSession(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Time

import DB
import Doc.SignatoryLinkID
import EID.CGI.GRP.Data
import KontraPrelude
import Session.SessionID

-- If one more type of a signature is to be added, follow the
-- convention, i.e. make constructor name the same as signature
-- type, but with underscore at the end (it would be best to
-- have no underscore, but we also want to export all the
-- signature types from this module and ghc complains about
-- ambiguous exports in such case).

data EAuthentication
  = BankIDAuthentication_ !BankIDAuthentication
  deriving (Eq, Ord, Show)

----------------------------------------

-- | Signature provider. Used internally to distinguish between
-- signatures in the database. Should not be exported, as the
-- distinction between various signatures on the outside should
-- be made with pattern matching on 'ESignature' constructors.
data AuthenticationProvider
  = CgiGrpBankID
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationProvider where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL AuthenticationProvider where
  type PQBase AuthenticationProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return CgiGrpBankID
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL AuthenticationProvider where
  type PQDest AuthenticationProvider = PQDest Int16
  toSQL CgiGrpBankID       = toSQL (1::Int16)

----------------------------------------

-- | Insert bank id authentication for a given signatory or replace the existing one.
data MergeBankIDAuthentication = MergeBankIDAuthentication SessionID SignatoryLinkID BankIDAuthentication
instance (MonadDB m, MonadMask m) => DBUpdate m MergeBankIDAuthentication () where
  update (MergeBankIDAuthentication sid slid BankIDAuthentication{..}) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_authentication" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        $unexpectedErrorM "signatory already signed, can't merge authentication"
      success <- runQuery01 . sqlUpdate "eid_authentications" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      when (not success) $ do
        runQuery_ . sqlInsertSelect "eid_authentications" "" $ do
          setFields
    where
      selectSignatorySignTime = do
        sqlSelect "signatory_links" $ do
          sqlResult "sign_time"
          sqlWhereEq "id" slid

      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "signatory_link_id" slid
        sqlSet "provider" CgiGrpBankID
        sqlSet "signature" bidaSignature
        sqlSet "signatory_name" bidaSignatoryName
        sqlSet "signatory_personal_number" bidaSignatoryPersonalNumber
        sqlSet "ocsp_response" bidaOcspResponse
        sqlSet "session_id" sid

-- | Get signature for a given signatory.
data GetEAuthenticationWithoutSession = GetEAuthenticationWithoutSession SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthenticationWithoutSession (Maybe EAuthentication) where
  query (GetEAuthenticationWithoutSession slid) = do
    runQuery_ . sqlSelect "eid_authentications" $ do
      sqlResult "provider"
      sqlResult "signature"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "ocsp_response"
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchESignature

-- | Get signature for a given signatory and session.
data GetEAuthentication = GetEAuthentication SessionID SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetEAuthentication (Maybe EAuthentication) where
  query (GetEAuthentication sid slid) = do
    runQuery_ . sqlSelect "eid_authentications" $ do
      sqlResult "provider"
      sqlResult "signature"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "ocsp_response"
      sqlWhereEq "session_id" sid
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchESignature

-- | Fetch e-signature.
fetchESignature :: (AuthenticationProvider, Binary ByteString, Text, Text, Binary ByteString) -> EAuthentication
fetchESignature (provider, signature, signatory_name, signatory_personal_number, ocsp_response) = case provider of
  CgiGrpBankID -> BankIDAuthentication_ BankIDAuthentication {
    bidaSignatoryName = signatory_name
  , bidaSignatoryPersonalNumber = signatory_personal_number
  , bidaSignature = signature
  , bidaOcspResponse = ocsp_response
  }
