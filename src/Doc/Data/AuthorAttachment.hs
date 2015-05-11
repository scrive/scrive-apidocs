module Doc.Data.AuthorAttachment (
    AuthorAttachment(..)
  , authorAttachmentsSelectors
  ) where

import DB
import File.FileID
import KontraPrelude

data AuthorAttachment = AuthorAttachment {
  authorattachmentfileid :: FileID,
  authorattachmentfilename :: String
} deriving (Eq, Ord, Show)

---------------------------------

authorAttachmentsSelectors :: [SQL]
authorAttachmentsSelectors = [
    "author_attachments.file_id",
    "files.name"
  ]

type instance CompositeRow AuthorAttachment = (FileID,String)

instance PQFormat AuthorAttachment where
  pqFormat _ = "%author_attachment"

instance CompositeFromSQL AuthorAttachment where
  toComposite (fid,fname) = AuthorAttachment {
    authorattachmentfileid = fid,
    authorattachmentfilename = fname
  }
