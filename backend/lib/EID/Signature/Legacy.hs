module EID.Signature.Legacy (
    LegacyBankIDSignature(..)
  , LegacyTeliaSignature(..)
  , LegacyNordeaSignature(..)
  , LegacyMobileBankIDSignature(..)
  ) where

import Data.ByteString (ByteString)

data LegacyBankIDSignature = LegacyBankIDSignature
  { lbidsSignedText  :: !Text
  , lbidsSignature   :: !ByteString
  , lbidsCertificate :: !ByteString
  } deriving (Eq, Ord, Show)

data LegacyTeliaSignature = LegacyTeliaSignature
  { ltsSignedText  :: !Text
  , ltsSignature   :: !ByteString
  , ltsCertificate :: !ByteString
  } deriving (Eq, Ord, Show)

data LegacyNordeaSignature = LegacyNordeaSignature
  { lnsSignedText  :: !Text
  , lnsSignature   :: !ByteString
  , lnsCertificate :: !ByteString
  } deriving (Eq, Ord, Show)

data LegacyMobileBankIDSignature = LegacyMobileBankIDSignature
  { lmbidsSignedText   :: !Text
  , lmbidsSignature    :: !ByteString
  , lmbidsOcspResponse :: !ByteString
  } deriving (Eq, Ord, Show)
