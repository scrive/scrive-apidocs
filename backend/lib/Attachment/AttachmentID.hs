module Attachment.AttachmentID (
    AttachmentID
  , unsafeAttachmentID
  ) where

import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server

newtype AttachmentID = AttachmentID Int64
  deriving (Eq, Ord, PQFormat)
deriving newtype instance Read AttachmentID
deriving newtype instance Show AttachmentID

instance FromReqURI AttachmentID where
  fromReqURI = maybeRead

instance Unjson AttachmentID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse AttachmentID")  return) . maybeRead) show  unjsonDef

instance FromSQL AttachmentID where
  type PQBase AttachmentID = PQBase Int64
  fromSQL mbase = AttachmentID <$> fromSQL mbase

instance ToSQL AttachmentID where
  type PQDest AttachmentID = PQDest Int64
  toSQL (AttachmentID n) = toSQL n

unsafeAttachmentID :: Int64 -> AttachmentID
unsafeAttachmentID = AttachmentID
