module Doc.API.V2.JSON.Document (
  unjsonDocument
, listToJSONBS
) where

import Control.Applicative.Free
import Data.ByteString.Builder
import Data.Default
import Data.Functor.Invariant
import Data.Unjson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Set as Set

import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.DisplayOptions
import Doc.API.V2.JSON.DocumentViewer
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.Utils
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import KontraLink
import KontraPrelude

unjsonDocument :: DocumentAccess -> UnjsonDef Document
unjsonDocument da = objectOf $
       pure def
  <*   fieldReadonly "id" documentid "Document ID"
  <**> (field "title" documenttitle "Document title"
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "signatories" documentsignatorylinks "Document signatories" (unjsonSignatoryArray da)
        <**> (pure (\sl d ->d { documentsignatorylinks = sl }) ))
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
        <**> (pure (\t d -> if t then d { documenttype = Template } else d)))
  <**> (field "is_saved" (not . documentunsaveddraft) "Whether document is saved"
        <**> (pure (\t d -> if t then d { documentunsaveddraft = False } else d)))
  <*   (fieldReadonly "is_shared" isDocumentShared "Whether document is shared")
  <*   (fieldReadonly "is_trashed" (propertyForCurrentSignatory da (isJust . signatorylinkdeleted)) "Whether document is in Trash")
  <*   (fieldReadonly "is_deleted" (propertyForCurrentSignatory da (isJust . signatorylinkreallydeleted)) "Whether document has been deleted")
  <*   (fieldReadonlyBy "viewer" (\d -> Just $ viewerForDocument da d) "Document viewer" unjsonDocumentViewer)

fieldAccessToken :: DocumentAccess -> Ap (FieldDef Document) ()
fieldAccessToken (DocumentAccess { daAccessMode }) =
  case daAccessMode of
       AuthorDocumentAccess -> accessTokenField
       AdminDocumentAccess -> accessTokenField
       _ -> pure ()
  where accessTokenField = fieldReadonly "access_token" documentmagichash "Document access token"

unjsonSignatoryArray ::  DocumentAccess -> UnjsonDef [SignatoryLink]
unjsonSignatoryArray da = arrayOf (unjsonSignatory da)

unjsonSignatory :: DocumentAccess -> UnjsonDef SignatoryLink
unjsonSignatory da =  objectOf $
       pure def
  <*   (fieldReadonly "id" signatorylinkid "Signatory ID")
  <*   (fieldReadOnlyOpt "user_id"  maybesignatory "User ID for the signatory")
  <*   (fieldReadonly "is_author" signatoryisauthor "Whether signatory is document author")
  <**> (fieldDef "is_signatory" (signatoryispartner def) signatoryispartner "Whether the signatory signs the document (or is a viewer)"
        <**> (pure $ \isa s -> s { signatoryispartner = isa }))
  <**> (fieldDefBy "fields"  (signatoryfields def) signatoryfields "Signatory fields" unjsonSignatoryFields
        <**> (pure $ \fs s -> s { signatoryfields = fs }))
  <**> (fieldDef "sign_order"  (signatorysignorder def) signatorysignorder "Signatory sign order"
        <**> (pure $ \so s-> s { signatorysignorder = so }))
  <*   (fieldReadOnlyOpt "sign_time" (fmap utcTimeToAPIFormat . fmap signtime . maybesigninfo) "Time when signatory signed the document")
  <*   (fieldReadOnlyOpt "seen_time" (fmap utcTimeToAPIFormat . fmap signtime . maybeseeninfo) "Time when signatory opened the document in browser" )
  <*   (fieldReadOnlyOpt "read_invitation_time" (fmap utcTimeToAPIFormat . maybereadinvite) "Time when signatory read invitation" )
  <*   (fieldReadOnlyOpt "rejected_time" (fmap utcTimeToAPIFormat . signatorylinkrejectiontime) "Time when signatory rejected the document" )
  <**> (fieldOpt "sign_success_redirect_url" signatorylinksignredirecturl ("URL to redirect the signatory after signing the document")
        <**> (pure $ \mrd s -> s { signatorylinksignredirecturl = mrd }))
  <**> (fieldOpt "reject_redirect_url" signatorylinkrejectredirecturl ("URL to redirect the signatory after rejecting the document")
        <**> (pure $ \mrd s -> s { signatorylinkrejectredirecturl = mrd }))
  <*   (fieldReadonlyBy "email_delivery_status" mailinvitationdeliverystatus "Email invitation delivery status" unjsonDeliveryStatus)
  <*   (fieldReadonlyBy "mobile_delivery_status" smsinvitationdeliverystatus "SMS invitation delivery status" unjsonDeliveryStatus)
  <**> (fieldOpt "csv" signatorylinkcsvupload ("CSV upload for multipart") <**> (pure $ \mcsv s -> s { signatorylinkcsvupload = mcsv })) -- Check only one csv for whole doc
  <**> (fieldDefBy "delivery_method" (signatorylinkdeliverymethod def) signatorylinkdeliverymethod "Signatory invitation delivery method" unjsonDeliveryMethod
        <**> (pure $ \sd s -> s { signatorylinkdeliverymethod = sd }))
  <**> (fieldDefBy "authentication_method_to_view" (signatorylinkauthenticationtoviewmethod def) signatorylinkauthenticationtoviewmethod "Signatory authentication to view method" unjsonAuthenticationToViewMethod
        <**> (pure $ \satv s -> s { signatorylinkauthenticationtoviewmethod = satv }))
  <**> (fieldDefBy "authentication_method_to_sign" (signatorylinkauthenticationtosignmethod def) signatorylinkauthenticationtosignmethod "Signatory authentication to sign method" unjsonAuthenticationToSignMethod
        <**> (pure $ \sats s -> s { signatorylinkauthenticationtosignmethod = sats }))
  <**> (fieldDefBy "confirmation_delivery_method" (signatorylinkconfirmationdeliverymethod def) signatorylinkconfirmationdeliverymethod "Signatory confirmation delivery method" unjsonConfirmationDeliveryMethod
        <**> (pure $ \scd s -> s { signatorylinkconfirmationdeliverymethod = scd }))
  <**> (fieldDefBy "attachments"  (signatoryattachments def) signatoryattachments "Signatory attachments" (arrayOf unjsonSignatoryAttachment)
        <**> (pure $ \sa s -> s { signatoryattachments = sa }))
  <*   (fieldReadOnlyOpt "api_delivery_url" (\sl ->
          if (daStatus da /= Preparation && signatorylinkdeliverymethod sl == APIDelivery && canSeeSignlinks da)
             then (Just $ show $ LinkSignDoc (daDocumentID da) sl)
             else Nothing
          ) "Link for signing document by API delivery"
       )

-- We can't implement lists as Unjson - since we would have to do unjsonToJSON on each document parser. And this will make us loose the order
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
