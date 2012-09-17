module Attachment.AttachmentID (
    AttachmentID
  , unsafeAttachmentID
  ) where

import Data.Int
import Happstack.Server

import DB.Derive
import Utils.Read

newtype AttachmentID = AttachmentID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''AttachmentID)

instance FromReqURI AttachmentID where
  fromReqURI = maybeRead

unsafeAttachmentID :: Int64 -> AttachmentID
unsafeAttachmentID = AttachmentID

$(newtypeDeriveConvertible ''AttachmentID)
