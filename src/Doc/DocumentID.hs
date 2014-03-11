module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  , fromDocumentID
  ) where

import Control.Applicative
import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import Utils.Read

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

instance FromReqURI DocumentID where
  fromReqURI = maybeRead

instance FromSQL DocumentID where
  type PQBase DocumentID = PQBase Int64
  fromSQL mbase = DocumentID <$> fromSQL mbase
instance ToSQL DocumentID where
  type PQDest DocumentID = PQDest Int64
  toSQL (DocumentID n) = toSQL n

unsafeDocumentID :: Int64 -> DocumentID
unsafeDocumentID = DocumentID

fromDocumentID :: DocumentID -> Int64
fromDocumentID (DocumentID did) = did
