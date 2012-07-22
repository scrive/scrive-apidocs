

module Attachment.Model
where

data Attachment = Attachment
  { attachmentid      :: AttachmentID
  , attachmenttitle   :: String
  , attachmentctime   :: MinutesTime
  , attachmentfile    :: FileID
  , attachmentshared  :: Bool
  , attachmentdeleted :: Bool
  }
