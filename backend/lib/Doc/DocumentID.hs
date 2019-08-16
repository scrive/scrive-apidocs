module Doc.DocumentID (
    DocumentID
  , unsafeDocumentID
  , fromDocumentID
  ) where

import Data.Binary as B
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype DocumentID = DocumentID Int64
  deriving (Eq, Ord)
deriving newtype instance Read DocumentID
deriving newtype instance Show DocumentID
deriving newtype instance TextShow DocumentID

instance PQFormat DocumentID where
  pqFormat = pqFormat @Int64

instance Identifier DocumentID where
  idDefaultLabel = "document_id"
  idValue        = int64AsStringIdentifier . fromDocumentID

instance FromReqURI DocumentID where
  fromReqURI = maybeRead . T.pack

instance Unjson DocumentID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse DocumentID") return) . maybeRead . T.pack) show unjsonDef

instance Binary DocumentID where
  put (DocumentID did) = put did
  get = fmap DocumentID B.get

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
