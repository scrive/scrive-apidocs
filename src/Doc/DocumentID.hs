module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  ) where

import Control.Monad
import Data.Data
import Data.Int

import Crypto.RNG
import DB.Derive
import Happstack.Data
import Happstack.Server
import Happstack.Util.Common

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

instance Random DocumentID where
  random = DocumentID `liftM` randomR (10000000, 10000000000)

instance FromReqURI DocumentID where
  fromReqURI = readM

$(deriveSerialize ''DocumentID)
instance Version DocumentID

unsafeDocumentID :: Int64 -> DocumentID
unsafeDocumentID = DocumentID
