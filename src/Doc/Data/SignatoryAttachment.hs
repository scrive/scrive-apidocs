module Doc.Data.SignatoryAttachment (
    SignatoryAttachment(..)
  , signatoryAttachmentsSelectors
  ) where

import DB
import File.FileID
import KontraPrelude

data SignatoryAttachment = SignatoryAttachment {
  signatoryattachmentfile     :: !(Maybe FileID)
, signatoryattachmentfilename :: !(Maybe String)
, signatoryattachmentname :: !String
, signatoryattachmentdescription :: !String
} deriving (Eq, Ord, Show)

---------------------------------

signatoryAttachmentsSelectors :: [SQL]
signatoryAttachmentsSelectors = [
    "signatory_attachments.file_id"
  , "files.name"
  , "signatory_attachments.name"
  , "signatory_attachments.description"
  ]

type instance CompositeRow SignatoryAttachment = (Maybe FileID, Maybe String, String, String)

instance PQFormat SignatoryAttachment where
  pqFormat _ = "%signatory_attachment"

instance CompositeFromSQL SignatoryAttachment where
  toComposite (mfid, mfname, name, description) = SignatoryAttachment {
    signatoryattachmentfile = mfid
  , signatoryattachmentfilename = mfname
  , signatoryattachmentname = name
  , signatoryattachmentdescription = description
  }
