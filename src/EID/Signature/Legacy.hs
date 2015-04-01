module EID.Signature.Legacy (
    LegacyBankIDSignature(..)
  , LegacyTeliaSignature(..)
  , LegacyNordeaSignature(..)
  , LegacyMobileBankIDSignature(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.PostgreSQL.PQTypes

import KontraPrelude

data LegacyBankIDSignature = LegacyBankIDSignature {
  lbidsSignedText  :: !Text
, lbidsSignature   :: !(Binary ByteString)
, lbidsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyTeliaSignature = LegacyTeliaSignature {
  ltsSignedText  :: !Text
, ltsSignature   :: !(Binary ByteString)
, ltsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyNordeaSignature = LegacyNordeaSignature {
  lnsSignedText  :: !Text
, lnsSignature   :: !(Binary ByteString)
, lnsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyMobileBankIDSignature = LegacyMobileBankIDSignature {
  lmbidsSignedText   :: !Text
, lmbidsSignature    :: !(Binary ByteString)
, lmbidsOcspResponse :: !(Binary ByteString)
} deriving (Eq, Ord, Show)
