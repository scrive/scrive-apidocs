
module Attachment.JSON
  ( unjsonAttachments
  , unjsonAttachmentSorting
  , unjsonAttachmentFiltering
  )
where

import Data.Unjson
import qualified Data.Text as T

import Attachment.Model
import DB
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.Utils

unjsonAttachments :: UnjsonDef [Attachment]
unjsonAttachments = objectOf $
  fieldBy "attachments" id "List of attachments" (arrayOf unjsonAttachment)

unjsonAttachment :: UnjsonDef Attachment
unjsonAttachment =  objectOf $
       pure def
  <*   (fieldReadonly "id" attachmentid "Attachment ID")
  <*   (fieldReadonly "title" attachmenttitle "User ID for the signatory")
  <*   (fieldReadonly "time" (utcTimeToAPIFormat . attachmentmtime) "Time of attachment creation")
  <*   (fieldReadonly "shared" attachmentshared "Is attachment shared")
  <*   (fieldReadonly "file" attachmentfile "Is attachment shared")


unjsonAttachmentSorting :: UnjsonDef [AscDesc AttachmentOrderBy]
unjsonAttachmentSorting = arrayOf $ objectOf $ pure (\f v -> (order f) v)
    <*> field "order" askDesc "Sorting value"
    <*> fieldBy "sort_by" sorting "Ask/Desc"  (unjsonEnumBy "Order" [ (AttachmentOrderByTitle, ("title" :: T.Text)), (AttachmentOrderByMTime, ("time" :: T.Text))])
  where
    order :: T.Text -> (a -> AscDesc a)
    order "ascending" = Asc
    order "descending" = Desc
    order _ = unexpectedError "parsing order for unjsonAttachmentSorting"
    askDesc (Asc _) = "ascending"
    askDesc (Desc _) = "descending"
    sorting (Asc o) = o
    sorting (Desc o) = o


unjsonAttachmentFiltering :: UnjsonDef [AttachmentFilter]
unjsonAttachmentFiltering =  arrayOf $ objectOf $ pure AttachmentFilterByString
  <*  fieldBy "filter_by" (const ()) "Type of filter" (unjsonEnum "Attachment filtering" textFilterParser (const "text"))
  <*> field "text" unsafeFilterText "Text to filter on"
  where
    unsafeFilterText :: AttachmentFilter ->  T.Text
    unsafeFilterText (AttachmentFilterByString text) = text
    unsafeFilterText _ = unexpectedError "unsafeDocumentAPIFilterText"
    textFilterParser "text" = Just ()
    textFilterParser _ = Nothing
