module Doc.Data.SignatoryAttachment (
    SignatoryAttachment(..)
  , signatoryAttachmentsSelectors
  ) where

import DB
import File.FileID
import KontraPrelude

data SignatoryAttachment = SignatoryAttachment {
  signatoryattachmentfile :: !(Maybe FileID)
, signatoryattachmentname :: !String
, signatoryattachmentdescription :: !String
} deriving (Eq, Ord, Show)

---------------------------------

signatoryAttachmentsSelectors :: [SQL]
signatoryAttachmentsSelectors = [
    "signatory_attachments.file_id"
  , "signatory_attachments.name"
  , "signatory_attachments.description"
  ]

type instance CompositeRow SignatoryAttachment = (Maybe FileID, String, String)

instance PQFormat SignatoryAttachment where
  pqFormat _ = "%signatory_attachment"

instance CompositeFromSQL SignatoryAttachment where
  toComposite (mfid, name, description) = SignatoryAttachment {
    signatoryattachmentfile = mfid
  , signatoryattachmentname = name
  , signatoryattachmentdescription = description
  }
