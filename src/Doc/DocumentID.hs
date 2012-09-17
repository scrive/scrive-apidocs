module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  ) where

import Data.Int
import Happstack.Server

import DB.Derive
import Utils.Read

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

instance FromReqURI DocumentID where
  fromReqURI = maybeRead

unsafeDocumentID :: Int64 -> DocumentID
unsafeDocumentID = DocumentID

$(newtypeDeriveConvertible ''DocumentID)
