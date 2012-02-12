

-- | This module contains definition of 'SignatoryLinkID' and
-- associated class implementations.  It is meant to be short module
-- just for this datatype so it can be included when nothing else is
-- needed.


module Doc.SignatoryLinkID 
  ( SignatoryLinkID(..)
  )

where

import DB.Derive
import Data.Data (Data)
import Data.Int
import Happstack.Data
import Happstack.Util.Common
import Happstack.Server.SimpleHTTP

-- | 'SignatoryLinkID' is and integer that identifies a signatory
-- inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int64 }
    deriving (Eq, Ord, Typeable, Data)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)
$(newtypeDeriveConvertible ''SignatoryLinkID)

instance FromReqURI SignatoryLinkID where
    fromReqURI = readM

instance Version SignatoryLinkID
$(deriveSerialize ''SignatoryLinkID)
