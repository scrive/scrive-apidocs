-- | This module contains definition of 'SignatoryLinkID' and
-- associated class implementations.  It is meant to be short module
-- just for this datatype so it can be included when nothing else is
-- needed.
module Doc.SignatoryLinkID (
    SignatoryLinkID
  , unsafeSignatoryLinkID
  ) where

import Data.Data
import Data.Int
import Data.SafeCopy
import Happstack.Server

import DB.Derive
import Misc

-- | 'SignatoryLinkID' is and integer that identifies
-- a signatory inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID Int64
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)
$(newtypeDeriveConvertible ''SignatoryLinkID)

$(deriveSafeCopy 0 'base ''SignatoryLinkID)

instance FromReqURI SignatoryLinkID where
  fromReqURI = maybeRead

unsafeSignatoryLinkID :: Int64 -> SignatoryLinkID
unsafeSignatoryLinkID = SignatoryLinkID
