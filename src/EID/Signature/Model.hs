module EID.Signature.Model where

import Data.ByteString (ByteString)

import DB
import EID.Signature.Provider
import EID.CGI.GRP.Signature

data ESignature
  = BankIDSignature BankIDSignature
  deriving (Eq, Ord, Show)

