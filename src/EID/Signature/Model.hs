{-# LANGUAGE NoImplicitPrelude #-}
module EID.Signature.Model (
    ESignature(..)
  -- from EID.CGI.GRP.Data
  , BankIDSignature(..)
  , InsertBankIDSignature(..)
  , GetESignature(..)
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Monoid.Space

import DB
import Doc.SignatoryLinkID
import EID.CGI.GRP.Data
import OurPrelude

-- If one more type of a signature is to be added, follow the
-- convention, i.e. make constructor name the same as signature
-- type, but with underscore at the end (it would be best to
-- have no underscore, but we also want to export all the
-- signature types from this module and ghc complains about
-- ambiguous exports in such case.

data ESignature
  = BankIDSignature_ !BankIDSignature
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
  pqFormat _ = pqFormat (undefined::Int16)

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

-- | Insert bank id signature for a given signatory.
data InsertBankIDSignature = InsertBankIDSignature SignatoryLinkID BankIDSignature
instance MonadDB m => DBUpdate m InsertBankIDSignature () where
  update (InsertBankIDSignature slid BankIDSignature{..}) = do
    runQuery_ . sqlInsert "eid_signatures" $ do
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

-- | Fetch e-signature. We currently do not fetch legacy signatures because they
-- are not used. If this is needed in the future, just implement needed cases.
fetchESignature :: (SignatureProvider, Text, Binary ByteString, Maybe (Binary ByteString), Maybe Text, Maybe Text, Maybe (Binary ByteString)) -> ESignature
fetchESignature (provider, sdata, signature, _mcertificate, msignatory_name, msignatory_personal_number, mocsp_response) = case provider of
  LegacyBankID -> not_implemented
  LegacyTelia -> not_implemented
  LegacyNordea -> not_implemented
  LegacyMobileBankID -> not_implemented
  CgiGrpBankID -> BankIDSignature_ BankIDSignature {
    bidsSignatoryName = $fromJust msignatory_name
  , bidsSignatoryPersonalNumber = $fromJust msignatory_personal_number
  , bidsSignedText = sdata
  , bidsSignature = signature
  , bidsOcspResponse = $fromJust mocsp_response
  }
  where
    not_implemented = $unexpectedError $ "fetching signature of type" <+> show provider <+> "is not implemented"
