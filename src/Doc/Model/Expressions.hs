module Doc.Model.Expressions (
    documentLatestSignTimeExpression
  , documentStatusClassExpression
  , documentSignorderExpression
  , statusClassCaseExpression
  , statusClassCaseExpressionForDocument
  , documentsSelectors
  , signatoryLinksSelectors
  , documentTagsSelectors
  , authorAttachmentsSelectors
  , mainFilesSelectors
  , signatoryAttachmentsSelectors
  , signatoryFieldsSelectors
  ) where

import Data.Monoid
import Data.Monoid.Space
import Data.Monoid.Utils

import DB
import Doc.DocStateData

documentLatestSignTimeExpression :: SQL
documentLatestSignTimeExpression = "(SELECT max(signatory_links.sign_time) FROM signatory_links WHERE signatory_links.document_id = documents.id)"

documentStatusClassExpression :: SQL
documentStatusClassExpression =
    "(SELECT COALESCE((SELECT min(" <> statusClassCaseExpression <> ")"
                     <> "FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner),"
                  <> "(SELECT " <> statusClassCaseExpressionForDocument <> "), "
                  <?> SCDraft <> "))::SMALLINT"

documentSignorderExpression :: SQL
documentSignorderExpression =
       "(COALESCE((SELECT min(signatory_links.sign_order) FROM signatory_links WHERE signatory_links.document_id = documents.id AND signatory_links.is_partner AND signatory_links.sign_time IS NULL), 1))"


statusClassCaseExpression :: SQL
statusClassCaseExpression =
  "(CASE"
   <+> "WHEN documents.status = " <?> (DocumentError "") <+> "THEN" <?> SCError
   <+> "WHEN documents.status = " <?> Preparation        <+> "THEN" <?> SCDraft
   <+> "WHEN signatory_links.sign_time IS NOT NULL THEN"            <?> SCSigned
   <+> "WHEN documents.status = " <?> Canceled           <+> "THEN" <?> SCCancelled
   <+> "WHEN documents.status = " <?> Timedout           <+> "THEN" <?> SCTimedout
   <+> "WHEN documents.status = " <?> Rejected           <+> "THEN" <?> SCRejected
   <+> "WHEN signatory_links.seen_time IS NOT NULL THEN"         <?> SCOpened
   <+> "WHEN signatory_links.read_invitation IS NOT NULL THEN"   <?> SCRead
   <+> "WHEN signatory_links.mail_invitation_delivery_status = " <?> Undelivered <+> "THEN" <?> SCDeliveryProblem
   <+> "WHEN signatory_links.sms_invitation_delivery_status = "  <?> Undelivered <+> "THEN" <?> SCDeliveryProblem
   <+> "WHEN signatory_links.mail_invitation_delivery_status = " <?> Delivered   <+> "THEN" <?> SCDelivered
   <+> "WHEN signatory_links.sms_invitation_delivery_status = "  <?> Delivered   <+> "THEN" <?> SCDelivered
   <+> "ELSE" <?> SCSent
  <+> "END :: SMALLINT)"

statusClassCaseExpressionForDocument :: SQL
statusClassCaseExpressionForDocument =
  "(CASE"
   <+> "WHEN documents.status = " <?> (DocumentError "") <+> "THEN" <?> SCError
   <+> "WHEN documents.status = " <?> Preparation        <+> "THEN" <?> SCDraft
   <+> "WHEN documents.status = " <?> Canceled           <+> "THEN" <?> SCCancelled
   <+> "WHEN documents.status = " <?> Timedout           <+> "THEN" <?> SCTimedout
   <+> "WHEN documents.status = " <?> Rejected           <+> "THEN" <?> SCRejected
  <+> "END :: INTEGER)"

documentsSelectors :: [SQL]
documentsSelectors = [
    "documents.id"
  , "documents.title"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryLinksSelectors <> ")::signatory_link FROM signatory_links WHERE documents.id = signatory_links.document_id ORDER BY signatory_links.id)"
  , "ARRAY(SELECT (" <> mintercalate ", " mainFilesSelectors <> ")::main_file FROM main_files WHERE documents.id = main_files.document_id ORDER BY main_files.id DESC)"
  , "documents.status"
  , "documents.error_text"
  , "documents.type"
  , "documents.ctime"
  , "documents.mtime"
  , "documents.days_to_sign"
  , "documents.days_to_remind"
  , "documents.timeout_time"
  , "(SELECT dar.expires FROM document_automatic_reminders dar WHERE dar.document_id = documents.id)"
  , "documents.invite_time"
  , "documents.invite_ip"
  , "documents.invite_text"
  , "documents.confirm_text"
  , "documents.show_header"
  , "documents.show_pdf_download"
  , "documents.show_reject_option"
  , "documents.show_footer"
  , "documents.lang"
  , "documents.sharing"
  , "ARRAY(SELECT (" <> mintercalate ", " documentTagsSelectors <> ")::document_tag FROM document_tags WHERE documents.id = document_tags.document_id ORDER BY document_tags.name)"
  , "ARRAY(SELECT author_attachments.file_id FROM author_attachments WHERE documents.id = author_attachments.document_id ORDER BY author_attachments.file_id)"
  , "documents.api_callback_url"
  , "documents.unsaved_draft"
  , "documents.object_version"
  , "documents.token"
  , "documents.time_zone_name"
  , "documents.api_version"
  , "(SELECT u.company_id FROM users u JOIN signatory_links sl ON u.id = sl.user_id WHERE sl.document_id = documents.id AND sl.is_author)"
  , documentStatusClassExpression
  ]

signatoryLinksSelectors :: [SQL]
signatoryLinksSelectors = [
    "signatory_links.id"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryFieldsSelectors <> ")::signatory_field FROM signatory_link_fields WHERE signatory_links.id = signatory_link_fields.signatory_link_id ORDER BY signatory_link_fields.id)"
  , "signatory_links.is_author"
  , "signatory_links.is_partner"
  , "signatory_links.sign_order"
  , "signatory_links.token"
  , "signatory_links.user_id"
  , "signatory_links.sign_time"
  , "signatory_links.sign_ip"
  , "signatory_links.seen_time"
  , "signatory_links.seen_ip"
  , "signatory_links.read_invitation"
  , "signatory_links.mail_invitation_delivery_status"
  , "signatory_links.sms_invitation_delivery_status"
  , "signatory_links.deleted"
  , "signatory_links.really_deleted"
  , "signatory_links.csv_title"
  , "signatory_links.csv_contents"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryAttachmentsSelectors <> ")::signatory_attachment FROM signatory_attachments WHERE signatory_links.id = signatory_attachments.signatory_link_id ORDER BY signatory_attachments.file_id)"
  , "signatory_links.sign_redirect_url"
  , "signatory_links.reject_redirect_url"
  , "signatory_links.rejection_time"
  , "signatory_links.rejection_reason"
  , "signatory_links.authentication_method"
  , "signatory_links.delivery_method"
  , "signatory_links.confirmation_delivery_method"
  ]

documentTagsSelectors :: [SQL]
documentTagsSelectors = [
    "document_tags.name"
  , "document_tags.value"
  ]

authorAttachmentsSelectors :: [SQL]
authorAttachmentsSelectors = [
    "author_attachments.file_id"
  ]

mainFilesSelectors :: [SQL]
mainFilesSelectors = [
    "main_files.file_id"
  , "main_files.document_status"
  , "main_files.seal_status"
  ]

signatoryAttachmentsSelectors :: [SQL]
signatoryAttachmentsSelectors = [
    "signatory_attachments.file_id"
  , "signatory_attachments.name"
  , "signatory_attachments.description"
  ]

signatoryFieldsSelectors :: [SQL]
signatoryFieldsSelectors = [
    "signatory_link_fields.id"
  , "signatory_link_fields.type"
  , "signatory_link_fields.custom_name"
  , "signatory_link_fields.is_author_filled"
  , "signatory_link_fields.value_text"
  , "signatory_link_fields.value_binary"
  , "signatory_link_fields.obligatory"
  , "signatory_link_fields.should_be_filled_by_author"
  , "signatory_link_fields.placements"
  ]
