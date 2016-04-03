module Doc.Data.AuthorAttachment (
    AuthorAttachment(..)
  , authorAttachmentsSelectors
  ) where

import DB
import File.FileID
import KontraPrelude

data AuthorAttachment = AuthorAttachment {
  authorattachmentname :: String,
  authorattachmentrequired :: Bool,
  authorattachmentfileid :: FileID
} deriving (Eq, Ord, Show)

---------------------------------

authorAttachmentsSelectors :: [SQL]
authorAttachmentsSelectors = [
    "author_attachments.name",
    "author_attachments.required",
    "author_attachments.file_id"
  ]

type instance CompositeRow AuthorAttachment = (String, Bool, FileID)

instance PQFormat AuthorAttachment where
  pqFormat _ = "%author_attachment"

instance CompositeFromSQL AuthorAttachment where
  toComposite (aname, areq,afid) = AuthorAttachment {
    authorattachmentname = aname,
    authorattachmentrequired = areq,
    authorattachmentfileid = afid
  }
