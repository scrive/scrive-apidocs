module Attachment.AttachmentID (
    AttachmentID
  , unsafeAttachmentID
  ) where

import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude
import Utils.Read

newtype AttachmentID = AttachmentID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''AttachmentID)

instance FromReqURI AttachmentID where
  fromReqURI = maybeRead

instance FromSQL AttachmentID where
  type PQBase AttachmentID = PQBase Int64
  fromSQL mbase = AttachmentID <$> fromSQL mbase

instance ToSQL AttachmentID where
  type PQDest AttachmentID = PQDest Int64
  toSQL (AttachmentID n) = toSQL n

unsafeAttachmentID :: Int64 -> AttachmentID
unsafeAttachmentID = AttachmentID
