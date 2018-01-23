module EID.Signature.Legacy (
    LegacyBankIDSignature(..)
  , LegacyTeliaSignature(..)
  , LegacyNordeaSignature(..)
  , LegacyMobileBankIDSignature(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T

data LegacyBankIDSignature = LegacyBankIDSignature {
  lbidsSignedText  :: !T.Text
, lbidsSignature   :: !ByteString
, lbidsCertificate :: !ByteString
} deriving (Eq, Ord, Show)

data LegacyTeliaSignature = LegacyTeliaSignature {
  ltsSignedText  :: !T.Text
, ltsSignature   :: !ByteString
, ltsCertificate :: !ByteString
} deriving (Eq, Ord, Show)

data LegacyNordeaSignature = LegacyNordeaSignature {
  lnsSignedText  :: !T.Text
, lnsSignature   :: !ByteString
, lnsCertificate :: !ByteString
} deriving (Eq, Ord, Show)

data LegacyMobileBankIDSignature = LegacyMobileBankIDSignature {
  lmbidsSignedText   :: !T.Text
, lmbidsSignature    :: !ByteString
, lmbidsOcspResponse :: !ByteString
} deriving (Eq, Ord, Show)
