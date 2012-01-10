

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
import Numeric

-- | 'SignatoryLinkID' is and integer that identifies a signatory
-- inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int64 }
    deriving (Eq, Ord, Typeable, Data)

instance Show SignatoryLinkID where
    showsPrec prec (SignatoryLinkID x) = showsPrec prec x

instance Read SignatoryLinkID where
    readsPrec _prec = let make (i,v) = (SignatoryLinkID i,v)
                      in map make . readSigned readDec

instance FromReqURI SignatoryLinkID where
    fromReqURI = readM

instance Version SignatoryLinkID

$(deriveSerialize ''SignatoryLinkID)
$(newtypeDeriveConvertible ''SignatoryLinkID)
