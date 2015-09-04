module EID.Signature.Legacy (
    LegacyBankIDSignature(..)
  , LegacyTeliaSignature(..)
  , LegacyNordeaSignature(..)
  , LegacyMobileBankIDSignature(..)
  ) where

import Data.ByteString (ByteString)
import Database.PostgreSQL.PQTypes
import qualified Data.Text as T

import KontraPrelude

data LegacyBankIDSignature = LegacyBankIDSignature {
  lbidsSignedText  :: !T.Text
, lbidsSignature   :: !(Binary ByteString)
, lbidsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyTeliaSignature = LegacyTeliaSignature {
  ltsSignedText  :: !T.Text
, ltsSignature   :: !(Binary ByteString)
, ltsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyNordeaSignature = LegacyNordeaSignature {
  lnsSignedText  :: !T.Text
, lnsSignature   :: !(Binary ByteString)
, lnsCertificate :: !(Binary ByteString)
} deriving (Eq, Ord, Show)

data LegacyMobileBankIDSignature = LegacyMobileBankIDSignature {
  lmbidsSignedText   :: !T.Text
, lmbidsSignature    :: !(Binary ByteString)
, lmbidsOcspResponse :: !(Binary ByteString)
} deriving (Eq, Ord, Show)
