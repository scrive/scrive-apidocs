module Doc.API.V2.JSON.Document (
  unjsonDocument
, listToJSONBS
) where

import Control.Applicative.Free
import Data.ByteString.Builder
import Data.Default
import Data.Functor.Invariant
import Data.String.Utils (strip)
import Data.Unjson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Set as Set

import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.DisplayOptions
import Doc.API.V2.JSON.DocumentViewer
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.API.V2.JSON.Utils
import Doc.DocInfo
import Doc.DocStateData
import KontraLink
import Util.SignatoryLinkUtils

unjsonDocument :: DocumentAccess -> UnjsonDef Document
unjsonDocument da = objectOf $
       pure def
  <*   fieldReadonly "id" documentid "Document ID"
  <**> (field "title" documenttitle "Document title"
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "parties" documentsignatorylinks "Document parties" (unjsonSignatoryArray da)
        <**> (pure (\sl d -> d { documentsignatorylinks = sl }) ))
  <*   fieldReadonlyBy "file" documentfile "Document main file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "sealed_file" documentsealedfile "Document sealed file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "author_attachments" documentauthorattachments "Document author attachments" (arrayOf unjsonAuthorAttachment)
  <*   fieldReadonly "ctime" (utcTimeToAPIFormat . documentctime) "Document creation time"
  <*   fieldReadonly "mtime" (utcTimeToAPIFormat . documentmtime) "Document modification time"
  <*   fieldReadOnlyOpt "timeout_time" (fmap utcTimeToAPIFormat . documenttimeouttime) "Document timeout time"
  <*   fieldReadOnlyOpt "auto_remind_time" (fmap utcTimeToAPIFormat . documentautoremindtime) "Document autoremind time"
  <*   fieldReadonlyBy "status" documentstatus "Document status" unjsonDocumentStatus
  <**> (field "days_to_sign" documentdaystosign "Days to sign document"
        <**> (pure $ \days d -> d { documentdaystosign = days }))
  <**> (fieldOpt "days_to_remind" documentdaystoremind "Days before reminding to sign document"
        <**> (pure $ \mdays d -> d { documentdaystoremind = mdays }))
  <**> (fieldBy "display_options" documentDisplayOptions "Document display options" unjsonDocumentDisplayOptions
        <**> (pure (applyDisplayOptionsToDocument)))
  <**> (field "invitation_message" documentinvitetext "Document invitation text"
        <**> (pure $ \t d -> d { documentinvitetext = t }))
  <**> (field "confirmation_message" documentconfirmtext "Document confirmation text"
        <**> (pure $ \t d -> d { documentconfirmtext = t }))
  <**> (fieldBy "lang" documentlang "Document language" unjsonLang
        <**> (pure $ \l d -> d { documentlang = l }))
  <**> (fieldOpt "api_callback_url" documentapiv2callbackurl "Document callback url (for V2)"
        <**> (pure $ \mcu d -> d { documentapiv2callbackurl = mcu }))
  <*   fieldReadonly "object_version" documentobjectversion "Document object version token"
  <*   fieldAccessToken da
  <**> (field "timezone" documenttimezonename "Document timezone"
        <**> (pure $ \tz d -> d { documenttimezonename = tz }))
  <**> (fieldBy "tags" documenttags "Document tags" (invmap Set.fromList Set.toList $ arrayOf unjsonDocumentTag)
        <**> (pure $ \tags d -> d { documenttags = tags }))
  <**> (field "is_template" isTemplate "Whether document is a template"
        <**> (pure (\t d -> d { documenttype = if t then Template else Signable } )))
  <**> (field "is_saved" (not . documentunsaveddraft) "Whether document is saved"
        <**> (pure (\t d -> d { documentunsaveddraft = not t } )))
  <*   (fieldReadonly "is_shared" isDocumentShared "Document is a template and is shared within company")
  <*   (fieldReadonly "is_trashed" (propertyForCurrentSignatory da (isJust . signatorylinkdeleted)) "Whether document is in Trash")
  <*   (fieldReadonly "is_deleted" (propertyForCurrentSignatory da (isJust . signatorylinkreallydeleted)) "Whether document has been deleted")
  <*   (fieldReadonlyBy "viewer" (\d -> Just $ viewerForDocument da d) "Document viewer" unjsonDocumentViewer)
  <*   fieldShareableLink da

fieldAccessToken :: DocumentAccess -> Ap (FieldDef Document) ()
fieldAccessToken (DocumentAccess { daAccessMode }) =
  case daAccessMode of
       AuthorDocumentAccess         -> accessTokenField
       CompanyAdminDocumentAccess _ -> accessTokenField
       CompanySharedDocumentAccess  -> pure ()
       SignatoryDocumentAccess _    -> pure ()
       SystemAdminDocumentAccess    -> pure ()
  where
    accessTokenField = fieldReadonly "access_token"
                       documentmagichash "Document access token"

fieldShareableLink :: DocumentAccess -> Ap (FieldDef Document) ()
fieldShareableLink DocumentAccess{ daAccessMode } =
     case daAccessMode of
       AuthorDocumentAccess         -> hashField
       CompanyAdminDocumentAccess _ -> hashField
       CompanySharedDocumentAccess  -> hashField
       SignatoryDocumentAccess _    -> pure ()
       SystemAdminDocumentAccess    -> pure ()
   where
     hashField :: Ap (FieldDef Document) ()
     hashField =
       fieldOpt "shareable_link"
                (\doc -> show . LinkTemplateShareableLink (documentid doc)
                         <$> documentshareablelinkhash doc)
                "Template's shareable link" *> pure ()

unjsonSignatoryArray ::  DocumentAccess -> UnjsonDef [SignatoryLink]
unjsonSignatoryArray da = arrayOf (unjsonSignatory da)

unjsonSignatory :: DocumentAccess -> UnjsonDef SignatoryLink
unjsonSignatory da =  objectOf $
  pure
  (\is_signatory msignatoryrole
    fields mbTitleQs signorder
    msuccredirecturl mrejredirecturl
    mcsvupload deliverymethod 
    authtoviewmethod authtoviewarchivedmethod
    authtosignmethod confirmdeliverymethod
    allowshighlighting hidepn sattachments ->
     let (title,qs) = maybe defTitleQs id mbTitleQs
         defTitleQs = ( signatorylinkconsenttitle def
                      , signatorylinkconsentquestions def )
         defSigRole = signatoryRoleFromBool is_signatory
     in def { signatoryrole                   = fromMaybe
                                                defSigRole msignatoryrole
            , signatoryfields                 = fields
            , signatorylinkconsenttitle       = title
            , signatorylinkconsentquestions   = qs
            , signatorysignorder              = signorder
            , signatorylinksignredirecturl    = fmap emptyIfNaughty
                                                msuccredirecturl
            , signatorylinkrejectredirecturl  = fmap emptyIfNaughty
                                                mrejredirecturl
            , signatorylinkcsvupload          = mcsvupload
              -- ^ Check only one csv for whole doc
            , signatorylinkdeliverymethod     = deliverymethod
            , signatorylinkauthenticationtoviewmethod
                                              = authtoviewmethod
            , signatorylinkauthenticationtoviewarchivedmethod
                                              = authtoviewarchivedmethod
            , signatorylinkauthenticationtosignmethod
                                              = authtosignmethod
            , signatorylinkconfirmationdeliverymethod
                                              = confirmdeliverymethod
            , signatorylinkallowshighlighting = allowshighlighting
            , signatorylinkhidepn             = hidepn
            , signatoryattachments            = sattachments
            })
  <*   (fieldReadonly "id" signatorylinkid "Signatory ID")
  <*   (fieldReadOnlyOpt "user_id"  maybesignatory "User ID for the signatory")
  <*   (fieldReadonly "is_author" signatoryisauthor "Whether signatory is document author")
  <*>  (fieldDef "is_signatory" (isSignatory (def :: SignatoryLink))
        isSignatory
        "Whether the signatory signs the document (or is a viewer/approver)")
  <*>  (fieldOptBy "signatory_role" (Just . signatoryrole)
        "Signatory role: 'viewer', 'approver', 'signing_party'."
        unjsonSignatoryRole)
  <*>  (fieldDefBy "fields"  (signatoryfields def) signatoryfields "Signatory fields" unjsonSignatoryFields)
  <*>  (fieldOptBy "consent_module"
         (\s -> if null (signatorylinkconsentquestions s)
                then Nothing
                else Just (signatorylinkconsenttitle s, signatorylinkconsentquestions s))
         "Signatory consent module" unjsonSignatoryConsentModule )
  <*>  (fieldDef "sign_order"  (signatorysignorder def) signatorysignorder "Signatory sign order")
  <*   (fieldReadOnlyOpt "sign_time" (fmap utcTimeToAPIFormat . fmap signtime . maybesigninfo) "Time when signatory signed the document")
  <*   (fieldReadOnlyOpt "seen_time" (fmap utcTimeToAPIFormat . fmap signtime . maybeseeninfo) "Time when signatory opened the document in browser" )
  <*   (fieldReadOnlyOpt "read_invitation_time" (fmap utcTimeToAPIFormat . maybereadinvite) "Time when signatory read invitation" )
  <*   (fieldReadOnlyOpt "rejected_time" (fmap utcTimeToAPIFormat . signatorylinkrejectiontime) "Time when signatory rejected the document" )
  <*   (fieldReadOnlyOpt "rejection_reason" signatorylinkrejectionreason "Message from the signatory to explain rejection")
  <*>  (fieldOpt "sign_success_redirect_url" signatorylinksignredirecturl ("URL to redirect the signatory after signing the document"))
  <*>  (fieldOpt "reject_redirect_url" signatorylinkrejectredirecturl ("URL to redirect the signatory after rejecting the document"))
  <*   (fieldReadonlyBy "email_delivery_status" mailinvitationdeliverystatus "Email invitation delivery status" unjsonDeliveryStatus)
  <*   (fieldReadonlyBy "mobile_delivery_status" smsinvitationdeliverystatus "SMS invitation delivery status" unjsonDeliveryStatus)
  <*   (fieldReadonly "has_authenticated_to_view" signatorylinkidentifiedtoview "Signatory has already authenticated to view")
  <*>  (fieldOptBy "csv" signatorylinkcsvupload "CSV upload for multipart" unjsonCSVUpload)
  <*>  (fieldDefBy "delivery_method" (signatorylinkdeliverymethod def) signatorylinkdeliverymethod "Signatory invitation delivery method" unjsonDeliveryMethod)
  <*>  (fieldDefBy "authentication_method_to_view" (signatorylinkauthenticationtoviewmethod def) signatorylinkauthenticationtoviewmethod "Signatory authentication to view method" unjsonAuthenticationToViewMethod)
  <*>  (fieldDefBy "authentication_method_to_view_archived" (signatorylinkauthenticationtoviewarchivedmethod def) signatorylinkauthenticationtoviewarchivedmethod "Signatory authentication to view archived method" unjsonAuthenticationToViewMethod)
  <*>  (fieldDefBy "authentication_method_to_sign" (signatorylinkauthenticationtosignmethod def) signatorylinkauthenticationtosignmethod "Signatory authentication to sign method" unjsonAuthenticationToSignMethod)
  <*>  (fieldDefBy "confirmation_delivery_method" (signatorylinkconfirmationdeliverymethod def) signatorylinkconfirmationdeliverymethod "Signatory confirmation delivery method" unjsonConfirmationDeliveryMethod)
  <*>  (fieldDef "allows_highlighting" (signatorylinkallowshighlighting def) signatorylinkallowshighlighting "Areas of main PDF can be highlighted during signing")
  <*>  (fieldDef "hide_personal_number" (signatorylinkhidepn def) signatorylinkhidepn "Signatory's personal number should be hidden")
  <*>  (fieldDefBy "attachments"  (signatoryattachments def) signatoryattachments "Signatory attachments" (arrayOf unjsonSignatoryAttachment))
  <*   (fieldReadonlyBy "highlighted_pages" signatoryhighlightedpages "Highlighted page during signing" (arrayOf unjsonHighlightedPage))
  <*   (fieldReadOnlyOpt "api_delivery_url" (\sl ->
          if (daStatus da /= Preparation && signatorylinkdeliverymethod sl == APIDelivery && canSeeSignlinks da)
             then (Just $ show $ LinkSignDoc (daDocumentID da) sl)
             else Nothing
          ) "Link for signing document by API delivery"
       )
  where
    emptyIfNaughty url = if any (\s -> s `isPrefixOf` (strip url)) ["javascript:","data:"] then "" else url

-- We can't implement lists as Unjson - since we would have to do
-- unjsonToJSON on each document parser. And that would make us lose
-- the order.
listToJSONBS ::  (Int,[(DocumentAccess,Document)]) -> BSC.ByteString
listToJSONBS (i,docs) = toLazyByteString $
    "{\"total_matching\":"  <> (lazyByteString  $ BSC.pack $ show i) <>
    ",\"documents\":[" <> (docList docs) <>
    "]}"
  where
    docList :: [(DocumentAccess,Document)] -> Builder
    docList [] = ""
    docList [l] = lazyByteString (unjsonDocDa l)
    docList (l:ls) = lazyByteString (unjsonDocDa l) <> "," <> docList ls
    unjsonDocDa (da,d) = unjsonToByteStringLazy' (Options {pretty = False, indent = 0, nulls = True}) (unjsonDocument da) d
