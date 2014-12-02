{-# LANGUAGE NoImplicitPrelude #-}
module EID.Signature.Model (
    ESignature(..)
  , BankIDSignature(..)
  , InsertBankIDSignature(..)
  , GetESignature(..)
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Monoid.Space

import DB
import Doc.SignatoryLinkID
import EID.Signature.Provider
import EID.CGI.GRP.Data
import OurPrelude

data ESignature
  = BankIDSignature_ !BankIDSignature
  deriving (Eq, Ord, Show)

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
