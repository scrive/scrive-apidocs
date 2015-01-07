module Doc.Data.AuthorAttachment (
    AuthorAttachment(..)
  , authorAttachmentsSelectors
  ) where

import DB
import File.FileID

newtype AuthorAttachment = AuthorAttachment {
  authorattachmentfile :: FileID
} deriving (Eq, Ord, Show)

---------------------------------

authorAttachmentsSelectors :: [SQL]
authorAttachmentsSelectors = [
    "author_attachments.file_id"
  ]

type instance CompositeRow AuthorAttachment = Single FileID

instance PQFormat AuthorAttachment where
  pqFormat _ = "%author_attachment"

instance CompositeFromSQL AuthorAttachment where
  toComposite (Single fid) = AuthorAttachment {
    authorattachmentfile = fid
  }
