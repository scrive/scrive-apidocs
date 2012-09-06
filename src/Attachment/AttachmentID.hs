module Attachment.AttachmentID (
    AttachmentID
  , unsafeAttachmentID
  ) where

import Data.Int
import Data.SafeCopy
import Happstack.Server

import DB.Derive
import Utils.Read

newtype AttachmentID = AttachmentID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''AttachmentID)

$(deriveSafeCopy 0 'base ''AttachmentID)

instance FromReqURI AttachmentID where
  fromReqURI = maybeRead

unsafeAttachmentID :: Int64 -> AttachmentID
unsafeAttachmentID = AttachmentID

$(newtypeDeriveConvertible ''AttachmentID)
