module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  ) where

import Control.Monad
import Data.Int
import Data.SafeCopy
import Happstack.Server

import Crypto.RNG
import DB.Derive
import Misc

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

$(deriveSafeCopy 0 'base ''DocumentID)

instance Random DocumentID where
  random = DocumentID `liftM` randomR (10000000, 10000000000)

instance FromReqURI DocumentID where
  fromReqURI = readM

unsafeDocumentID :: Int64 -> DocumentID
unsafeDocumentID = DocumentID
