module Doc.Types.SigningData (
    SignatorySigningData(..)
 ) where

import Data.Data

import Doc.Types.SignatoryLink
import EID.Signature.Model

data SignatorySigningData = SignatorySigningData
  { ssdHasSigned :: Bool
  , ssdData :: Either AuthenticationToSignMethod ESignature
  } deriving (Show, Typeable)
