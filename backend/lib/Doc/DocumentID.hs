module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  , fromDocumentID
  ) where

import Data.Aeson
import Data.Binary
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude
import Log.Identifier

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

instance Identifier DocumentID Int64 where
  idDefaultLabel _ = "document_id"
  idValue = toJSON . fromDocumentID

instance FromReqURI DocumentID where
  fromReqURI = maybeRead

instance Unjson DocumentID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) show unjsonDef

instance Binary DocumentID where
  put (DocumentID did) = put did
  get = fmap DocumentID get

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
