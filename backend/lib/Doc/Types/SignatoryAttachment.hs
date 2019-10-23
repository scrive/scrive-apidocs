module Doc.Types.SignatoryAttachment (
    SignatoryAttachment(..)
  , defaultSignatoryAttachment
  , signatoryAttachmentsSelectors
  ) where

import DB
import Doc.Tables
import File.FileID

data SignatoryAttachment = SignatoryAttachment {
  signatoryattachmentfile     :: !(Maybe FileID)
, signatoryattachmentfilename :: !(Maybe Text)
, signatoryattachmentname :: !Text
, signatoryattachmentdescription :: !Text
, signatoryattachmentrequired :: !Bool
} deriving (Eq, Ord, Show)

---------------------------------

signatoryAttachmentsSelectors :: [SQL]
signatoryAttachmentsSelectors =
  [ "signatory_attachments.file_id"
  , "files.name"
  , "signatory_attachments.name"
  , "signatory_attachments.description"
  , "signatory_attachments.required"
  ]

type instance CompositeRow SignatoryAttachment
  = (Maybe FileID, Maybe Text, Text, Text, Bool)

instance PQFormat SignatoryAttachment where
  pqFormat = compositeTypePqFormat ctSignatoryAttachment

instance CompositeFromSQL SignatoryAttachment where
  toComposite (mfid, mfname, name, description, required) = SignatoryAttachment
    { signatoryattachmentfile        = mfid
    , signatoryattachmentfilename    = mfname
    , signatoryattachmentname        = name
    , signatoryattachmentdescription = description
    , signatoryattachmentrequired    = required
    }

defaultSignatoryAttachment :: SignatoryAttachment
defaultSignatoryAttachment =
  SignatoryAttachment Nothing Nothing "att1_name" "att1_desc" True
