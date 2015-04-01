module EID.Signature.Model (
    ESignature(..)
  , module EID.Signature.Legacy
  -- from EID.CGI.GRP.Data
  , BankIDSignature(..)
  , MergeBankIDSignature(..)
  , GetESignature(..)
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
import EID.Signature.Legacy
import KontraPrelude

-- If one more type of a signature is to be added, follow the
-- convention, i.e. make constructor name the same as signature
-- type, but with underscore at the end (it would be best to
-- have no underscore, but we also want to export all the
-- signature types from this module and ghc complains about
-- ambiguous exports in such case).

data ESignature
  = LegacyBankIDSignature_ !LegacyBankIDSignature
  | LegacyTeliaSignature_ !LegacyTeliaSignature
  | LegacyNordeaSignature_ !LegacyNordeaSignature
  | LegacyMobileBankIDSignature_ !LegacyMobileBankIDSignature
  | BankIDSignature_ !BankIDSignature
  deriving (Eq, Ord, Show)

----------------------------------------

-- | Signature provider. Used internally to distinguish between
-- signatures in the database. Should not be exported, as the
-- distinction between various signatures on the outside should
-- be made with pattern matching on 'ESignature' constructors.
data SignatureProvider
  = LegacyBankID
  | LegacyTelia
  | LegacyNordea
  | LegacyMobileBankID
  | CgiGrpBankID
    deriving (Eq, Ord, Show)

instance PQFormat SignatureProvider where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return LegacyBankID
      2 -> return LegacyTelia
      3 -> return LegacyNordea
      4 -> return LegacyMobileBankID
      5 -> return CgiGrpBankID
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL LegacyBankID       = toSQL (1::Int16)
  toSQL LegacyTelia        = toSQL (2::Int16)
  toSQL LegacyNordea       = toSQL (3::Int16)
  toSQL LegacyMobileBankID = toSQL (4::Int16)
  toSQL CgiGrpBankID       = toSQL (5::Int16)

----------------------------------------

-- | Insert bank id signature for a given signatory or replace the existing one.
data MergeBankIDSignature = MergeBankIDSignature SignatoryLinkID BankIDSignature
instance (MonadDB m, MonadMask m) => DBUpdate m MergeBankIDSignature () where
  update (MergeBankIDSignature slid BankIDSignature{..}) = do
    loopOnUniqueViolation . withSavepoint "merge_bank_id_signature" $ do
      runQuery01_ selectSignatorySignTime
      msign_time :: Maybe UTCTime <- fetchOne runIdentity
      when (isJust msign_time) $ do
        $unexpectedErrorM "signatory already signed, can't merge signature"
      success <- runQuery01 . sqlUpdate "eid_signatures" $ do
        setFields
        sqlWhereEq "signatory_link_id" slid
        -- replace the signature only if signatory hasn't signed yet
        sqlWhere $ parenthesize (toSQLCommand selectSignatorySignTime) <+> "IS NULL"
      when (not success) $ do
        runQuery_ . sqlInsertSelect "eid_signatures" "" $ do
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
        sqlSet "data" bidsSignedText
        sqlSet "signature" bidsSignature
        sqlSet "signatory_name" bidsSignatoryName
        sqlSet "signatory_personal_number" bidsSignatoryPersonalNumber
        sqlSet "ocsp_response" bidsOcspResponse

-- | Get signature for a given signatory.
data GetESignature = GetESignature SignatoryLinkID
instance (MonadThrow m, MonadDB m) => DBQuery m GetESignature (Maybe ESignature) where
  query (GetESignature slid) = do
    runQuery_ . sqlSelect "eid_signatures" $ do
      sqlResult "provider"
      sqlResult "data"
      sqlResult "signature"
      sqlResult "certificate"
      sqlResult "signatory_name"
      sqlResult "signatory_personal_number"
      sqlResult "ocsp_response"
      sqlWhereEq "signatory_link_id" slid
    fetchMaybe fetchESignature

-- | Fetch e-signature.
fetchESignature :: (SignatureProvider, Text, Binary ByteString, Maybe (Binary ByteString), Maybe Text, Maybe Text, Maybe (Binary ByteString)) -> ESignature
fetchESignature (provider, sdata, signature, mcertificate, msignatory_name, msignatory_personal_number, mocsp_response) = case provider of
  LegacyBankID -> LegacyBankIDSignature_ LegacyBankIDSignature {
    lbidsSignedText = sdata
  , lbidsSignature = signature
  , lbidsCertificate = $fromJust mcertificate
  }
  LegacyTelia -> LegacyTeliaSignature_ LegacyTeliaSignature {
    ltsSignedText = sdata
  , ltsSignature = signature
  , ltsCertificate = $fromJust mcertificate
  }
  LegacyNordea -> LegacyNordeaSignature_ LegacyNordeaSignature {
    lnsSignedText = sdata
  , lnsSignature = signature
  , lnsCertificate = $fromJust mcertificate
  }
  LegacyMobileBankID -> LegacyMobileBankIDSignature_ LegacyMobileBankIDSignature {
    lmbidsSignedText = sdata
  , lmbidsSignature = signature
  , lmbidsOcspResponse = $fromJust mocsp_response
  }
  CgiGrpBankID -> BankIDSignature_ BankIDSignature {
    bidsSignatoryName = $fromJust msignatory_name
  , bidsSignatoryPersonalNumber = $fromJust msignatory_personal_number
  , bidsSignedText = sdata
  , bidsSignature = signature
  , bidsOcspResponse = $fromJust mocsp_response
  }
