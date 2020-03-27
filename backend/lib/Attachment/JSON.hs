module Attachment.JSON
  ( unjsonAttachments
  , unjsonAttachmentSorting
  , unjsonAttachmentFiltering
  ) where

import Data.Unjson

import Attachment.Model
import DB
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.Utils

unjsonAttachments :: UnjsonDef [Attachment]
unjsonAttachments = objectOf
  $ fieldBy "attachments" identity "List of attachments" (arrayOf unjsonAttachment)

unjsonAttachment :: UnjsonDef Attachment
unjsonAttachment =
  objectOf
    $  defaultAttachment
    <$ fieldReadonly "id"    attachmentid    "Attachment ID"
    <* fieldReadonly "title" attachmenttitle "User ID for the signatory"
    <* fieldReadonly "time"
                     (utcTimeToAPIFormat . attachmentmtime)
                     "Time of attachment creation"
    <* fieldReadonly "shared" attachmentshared "Is attachment shared"
    <* fieldReadonly "file"   attachmentfile   "Is attachment shared"

{-# ANN unjsonAttachmentSorting ("HLint: ignore" :: String) #-}
unjsonAttachmentSorting :: UnjsonDef [AscDesc AttachmentOrderBy]
unjsonAttachmentSorting =
  arrayOf
    .   objectOf
    $   order
    <$> (field "order" askDesc "Sorting value")
    <*> (fieldBy "sort_by" sorting "Asc/Desc" $ unjsonEnumBy
          "Order"
          [ (AttachmentOrderByTitle, "title" :: Text)
          , (AttachmentOrderByMTime, "time" :: Text)
          ]
        )
  where
    order :: Text -> (a -> AscDesc a)
    order "ascending"  = Asc
    order "descending" = Desc
    order _            = unexpectedError "parsing order for unjsonAttachmentSorting"
    askDesc (Asc  _) = "ascending"
    askDesc (Desc _) = "descending"
    sorting (Asc  o) = o
    sorting (Desc o) = o

unjsonAttachmentFiltering :: UnjsonDef [AttachmentFilter]
unjsonAttachmentFiltering =
  arrayOf
    .   objectOf
    $   AttachmentFilterByString
    <$  fieldBy "filter_by"
                (const ())
                "Type of filter"
                (unjsonEnum "Attachment filtering" textFilterParser (const "text"))
    <*> field "text" unsafeFilterText "Text to filter on"

  where
    unsafeFilterText :: AttachmentFilter -> Text
    unsafeFilterText (AttachmentFilterByString text) = text
    unsafeFilterText _ = unexpectedError "unsafeDocumentAPIFilterText"
    textFilterParser "text" = Just ()
    textFilterParser _      = Nothing
