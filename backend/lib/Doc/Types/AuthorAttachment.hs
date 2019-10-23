module Doc.Types.AuthorAttachment (
    AuthorAttachment(..)
  , authorAttachmentsSelectors
  ) where

import DB
import Doc.Tables
import File.FileID

data AuthorAttachment = AuthorAttachment {
  authorattachmentname :: Text,
  authorattachmentrequired :: Bool,
  authorattachmentaddtosealedfile :: Bool,
  authorattachmentfileid :: FileID
} deriving (Eq, Ord, Show)

---------------------------------

authorAttachmentsSelectors :: [SQL]
authorAttachmentsSelectors =
  [ "author_attachments.name"
  , "author_attachments.required"
  , "author_attachments.add_to_sealed_file"
  , "author_attachments.file_id"
  ]

type instance CompositeRow AuthorAttachment = (Text, Bool, Bool, FileID)

instance PQFormat AuthorAttachment where
  pqFormat = compositeTypePqFormat ctAuthorAttachment

instance CompositeFromSQL AuthorAttachment where
  toComposite (aname, areq, aatsf, afid) = AuthorAttachment
    { authorattachmentname            = aname
    , authorattachmentrequired        = areq
    , authorattachmentaddtosealedfile = aatsf
    , authorattachmentfileid          = afid
    }
