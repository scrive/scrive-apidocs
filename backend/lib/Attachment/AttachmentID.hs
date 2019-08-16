module Attachment.AttachmentID (
    AttachmentID
  , unsafeAttachmentID
  ) where

import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

newtype AttachmentID = AttachmentID Int64
  deriving (Eq, Ord)
deriving newtype instance Read AttachmentID
deriving newtype instance Show AttachmentID

instance PQFormat AttachmentID where
  pqFormat = pqFormat @Int64

instance FromReqURI AttachmentID where
  fromReqURI = maybeRead . T.pack

instance Unjson AttachmentID where
  unjsonDef = unjsonInvmapR
              ((maybe (fail "Can't parse AttachmentID")  return) . maybeRead . T.pack)
              show  unjsonDef

instance FromSQL AttachmentID where
  type PQBase AttachmentID = PQBase Int64
  fromSQL mbase = AttachmentID <$> fromSQL mbase

instance ToSQL AttachmentID where
  type PQDest AttachmentID = PQDest Int64
  toSQL (AttachmentID n) = toSQL n

unsafeAttachmentID :: Int64 -> AttachmentID
unsafeAttachmentID = AttachmentID
