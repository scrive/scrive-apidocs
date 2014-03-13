module Doc.Model.Expressions
  ( documentLatestSignTimeExpression
  , documentStatusClassExpression
  , documentSignorderExpression
  , statusClassCaseExpression
  , statusClassCaseExpressionForDocument
  , selectSignatoryLinksX
  , fetchSignatoryLinks
  , fetchDocumentTags
  , fetchAuthorAttachments
  , mainFilesSelectors
  , fetchMainFiles
  , fetchSignatoryAttachments
  , signatoryLinkFieldsSelectors
  , fetchSignatoryLinkFields
  ) where

import Control.Applicative
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Monoid.Space
import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import OurPrelude
import Prelude hiding (head, tail)

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
   <+> "WHEN documents.status = " <?> Canceled           <+> "THEN" <?> SCCancelled
   <+> "WHEN documents.status = " <?> Timedout           <+> "THEN" <?> SCTimedout
   <+> "WHEN documents.status = " <?> Rejected           <+> "THEN" <?> SCRejected
   <+> "WHEN signatory_links.sign_time IS NOT NULL THEN"         <?> SCSigned
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

selectSignatoryLinksX :: State.State SqlSelect () -> SqlSelect
selectSignatoryLinksX extension = sqlSelect "signatory_links" $ do
  sqlResult "signatory_links.id"
  sqlResult "signatory_links.document_id"
  sqlResult "signatory_links.user_id"
  sqlResult "signatory_links.sign_order"
  sqlResult "signatory_links.token"
  sqlResult "signatory_links.sign_time"
  sqlResult "signatory_links.sign_ip"
  sqlResult "signatory_links.seen_time"
  sqlResult "signatory_links.seen_ip"
  sqlResult "signatory_links.read_invitation"
  sqlResult "signatory_links.mail_invitation_delivery_status"
  sqlResult "signatory_links.sms_invitation_delivery_status"
  sqlResult "signatory_links.signinfo_text"
  sqlResult "signatory_links.signinfo_signature"
  sqlResult "signatory_links.signinfo_certificate"
  sqlResult "signatory_links.signinfo_provider"
  sqlResult "signatory_links.signinfo_first_name_verified"
  sqlResult "signatory_links.signinfo_last_name_verified"
  sqlResult "signatory_links.signinfo_personal_number_verified"
  sqlResult "signatory_links.signinfo_ocsp_response"
  sqlResult "signatory_links.is_author"
  sqlResult "signatory_links.is_partner"
  sqlResult "signatory_links.csv_title"
  sqlResult "signatory_links.csv_contents"
  sqlResult "signatory_links.deleted"
  sqlResult "signatory_links.really_deleted"
  sqlResult "signatory_links.sign_redirect_url"
  sqlResult "signatory_links.reject_redirect_url"
  sqlResult "signatory_links.rejection_time"
  sqlResult "signatory_links.rejection_reason"
  sqlResult "signatory_links.authentication_method"
  sqlResult "signatory_links.eleg_data_mismatch_message"
  sqlResult "signatory_links.eleg_data_mismatch_first_name"
  sqlResult "signatory_links.eleg_data_mismatch_last_name"
  sqlResult "signatory_links.eleg_data_mismatch_personal_number"
  sqlResult "signatory_links.delivery_method"
  sqlResult "signatory_links.confirmation_delivery_method"

  sqlResult (statusClassCaseExpression <> " AS status_class")
  sqlResult "signatory_attachments.file_id AS sigfileid"
  sqlResult "signatory_attachments.name AS signame"
  sqlResult "signatory_attachments.description AS sigdesc"
  sqlLeftJoinOn "signatory_attachments" "signatory_attachments.signatory_link_id = signatory_links.id"
  sqlJoinOn "documents" "signatory_links.document_id = documents.id"
  extension

fetchSignatoryLinks :: MonadDB m => m (M.Map DocumentID [SignatoryLink])
fetchSignatoryLinks = do
  sigs <- foldlM decoder (nulldocid, [], M.empty)
  return $ (\(d, l, m) -> M.insertWith' (++) d l m) sigs
  where
    nulldocid = unsafeDocumentID $ -1
    decoder (docid, links, linksmap) (slid, document_id, user_id,
     sign_order, token, sign_time, sign_ip, seen_time, seen_ip, read_invitation,
     mail_invitation_delivery_status, sms_invitation_delivery_status, signinfo_text, signinfo_signature, signinfo_certificate,
     signinfo_provider, signinfo_first_name_verified, signinfo_last_name_verified,
     signinfo_personal_number_verified, signinfo_ocsp_response,
     is_author, is_partner, csv_title, csv_contents,
     deleted, really_deleted, signredirecturl, rejectredirecturl,
     rejection_time, rejection_reason,
     authentication_method,
     eleg_data_mismatch_message,
     eleg_data_mismatch_first_name,
     eleg_data_mismatch_last_name,
     eleg_data_mismatch_personal_number,
     delivery_method,
     confirmation_delivery_method,
     status_class,
     safileid, saname, sadesc)
      | docid == nulldocid                      = return (document_id, [link], linksmap)
      | docid /= document_id                    = return (document_id, [link], M.insertWith' (++) docid links linksmap)
      | signatorylinkid ($(head) links) == slid = return (docid, addSigAtt ($(head) links) : $(tail) links, linksmap)
      | otherwise                               = return (docid, link : links, linksmap)
      where
        addSigAtt l = l { signatoryattachments = sigAtt ++ signatoryattachments l }
        sigAtt = maybe [] (\name -> [SignatoryAttachment {
            signatoryattachmentfile = safileid
          , signatoryattachmentname = name
          , signatoryattachmentdescription = fromMaybe "" sadesc
          }]) saname
        link = SignatoryLink {
            signatorylinkid = slid
          , signatorysignorder = sign_order
          , signatoryfields = []
          , signatoryisauthor = is_author
          , signatoryispartner = is_partner
          , signatorymagichash = token
          , maybesignatory = user_id
          , maybesigninfo = SignInfo <$> sign_time <*> sign_ip
          , maybeseeninfo = SignInfo <$> seen_time <*> seen_ip
          , maybereadinvite = read_invitation
          , mailinvitationdeliverystatus = mail_invitation_delivery_status
          , smsinvitationdeliverystatus = sms_invitation_delivery_status
          , signatorysignatureinfo = do -- Maybe Monad
              signinfo_text' <- signinfo_text
              signinfo_signature' <- signinfo_signature
              signinfo_certificate' <- signinfo_certificate
              signinfo_provider' <- signinfo_provider
              signinfo_first_name_verified' <- signinfo_first_name_verified
              signinfo_last_name_verified' <- signinfo_last_name_verified
              signinfo_personal_number_verified' <- signinfo_personal_number_verified
              return $ SignatureInfo {
                  signatureinfotext        = signinfo_text'
                , signatureinfosignature   = signinfo_signature'
                , signatureinfocertificate = signinfo_certificate'
                , signatureinfoprovider    = signinfo_provider'
                , signaturefstnameverified = signinfo_first_name_verified'
                , signaturelstnameverified = signinfo_last_name_verified'
                , signaturepersnumverified = signinfo_personal_number_verified'
                , signatureinfoocspresponse = signinfo_ocsp_response
                }
          , signatorylinkdeleted = deleted
          , signatorylinkreallydeleted = really_deleted
          , signatorylinkcsvupload =
              CSVUpload <$> csv_title <*> csv_contents
          , signatoryattachments = sigAtt
          , signatorylinkstatusclass = status_class
          , signatorylinksignredirecturl = signredirecturl
          , signatorylinkrejectredirecturl = rejectredirecturl
          , signatorylinkrejectionreason = rejection_reason
          , signatorylinkrejectiontime = rejection_time
          , signatorylinkauthenticationmethod = authentication_method
          , signatorylinkelegdatamismatchmessage = eleg_data_mismatch_message
          , signatorylinkelegdatamismatchfirstname = eleg_data_mismatch_first_name
          , signatorylinkelegdatamismatchlastname = eleg_data_mismatch_last_name
          , signatorylinkelegdatamismatchpersonalnumber = eleg_data_mismatch_personal_number
          , signatorylinkdeliverymethod = delivery_method
          , signatorylinkconfirmationdeliverymethod = confirmation_delivery_method
          }

fetchDocumentTags :: MonadDB m => m (M.Map DocumentID (S.Set DocumentTag))
fetchDocumentTags = foldlM decoder M.empty
  where
    decoder acc (document_id, name, v) = return $
      M.insertWith' S.union document_id
         (S.singleton $ DocumentTag name v) acc

fetchAuthorAttachments :: MonadDB m => m (M.Map DocumentID [AuthorAttachment])
fetchAuthorAttachments = foldlM decoder M.empty
  where
    decoder acc (document_id, file_id) = return $
      M.insertWith' (++) document_id [AuthorAttachment {
        authorattachmentfile = file_id
      }] acc

mainFilesSelectors :: [SQL]
mainFilesSelectors =
  [ "document_id"
  , "file_id"
  , "document_status"
  , "seal_status"
  ]

fetchMainFiles :: MonadDB m => m (M.Map DocumentID [MainFile])
fetchMainFiles = foldlM decoder M.empty
  where
    decoder acc (document_id, file_id, document_status, seal_status) = return $
      M.insertWith' (++) document_id [MainFile {
        mainfileid = file_id
      , mainfiledocumentstatus = document_status
      , mainfilesealstatus = seal_status
      }] acc

fetchSignatoryAttachments :: MonadDB m => m (M.Map SignatoryLinkID [SignatoryAttachment])
fetchSignatoryAttachments = foldlM decoder M.empty
  where
    decoder acc (signatory_link_id, file_id, name, description) = return $
      M.insertWith' (++) signatory_link_id [SignatoryAttachment {
          signatoryattachmentfile = file_id
        , signatoryattachmentname = name
        , signatoryattachmentdescription = description
        }] acc


signatoryLinkFieldsSelectors :: [SQL]
signatoryLinkFieldsSelectors =
  [ "signatory_link_id"
  , "type"
  , "custom_name"
  , "is_author_filled"
  , "value"
  , "obligatory"
  , "should_be_filled_by_author"
  , "placements"
  ]

fetchSignatoryLinkFields :: MonadDB m => m (M.Map SignatoryLinkID [SignatoryField])
fetchSignatoryLinkFields = foldlM decoder M.empty
  where
    decoder acc (slid, xtype, custom_name, is_author_filled, v, obligatory, should_be_filled_by_sender, placements) = return $
      M.insertWith' (++) slid
         [SignatoryField
          { sfValue = v
          , sfPlacements = placements
          , sfType = case xtype of
                        CustomFT{} -> CustomFT custom_name is_author_filled
                        CheckboxFT{} -> CheckboxFT custom_name
                        SignatureFT{} -> SignatureFT (if null custom_name
                                                      then "signature"
                                                      else custom_name)
                        _   -> xtype
          , sfObligatory = obligatory
          , sfShouldBeFilledBySender = should_be_filled_by_sender
          }] acc

