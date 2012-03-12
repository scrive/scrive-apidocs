-- | This module contains definition of 'SignatoryLinkID' and
-- associated class implementations.  It is meant to be short module
-- just for this datatype so it can be included when nothing else is
-- needed.
module Doc.SignatoryLinkID (
    SignatoryLinkID
  , unsafeSignatoryLinkID
  ) where

import Control.Monad
import Data.Data
import Data.Int

import Crypto.RNG
import DB.Derive
import Happstack.Data
import Happstack.Server
import Happstack.Util.Common

-- | 'SignatoryLinkID' is and integer that identifies
-- a signatory inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID Int64
  deriving (Eq, Ord, Typeable, Data)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)
$(newtypeDeriveConvertible ''SignatoryLinkID)

instance Random SignatoryLinkID where
  random = SignatoryLinkID `liftM` randomR (10000000, 10000000000)

instance FromReqURI SignatoryLinkID where
  fromReqURI = readM

instance Version SignatoryLinkID
$(deriveSerialize ''SignatoryLinkID)

unsafeSignatoryLinkID :: Int64 -> SignatoryLinkID
unsafeSignatoryLinkID = SignatoryLinkID
