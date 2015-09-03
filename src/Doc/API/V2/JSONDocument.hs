module Doc.API.V2.JSONDocument (
  unjsonDocument
, listToJSONBS
) where

import Control.Applicative.Free
import Data.ByteString.Lazy.Char8 hiding (map, last)
import Data.Default
import Data.Unjson

import Doc.API.V2.DisplayOptions
import Doc.API.V2.DocumentAccess
import Doc.API.V2.DocumentViewer
import Doc.API.V2.JSONFields
import Doc.API.V2.JSONMisc
import Doc.API.V2.UnjsonUtils
import Doc.DocStateData
import Doc.DocUtils
import KontraLink
import KontraPrelude

unjsonDocument :: DocumentAccess -> UnjsonDef Document
unjsonDocument da = objectOf $
       pure def
  <*   fieldReadonly "id" (documentid) "Document id "
  <**> (field "title" documenttitle ("Document title")
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "signatories" (documentsignatorylinks) ("Document signatories") (unjsonSignatoryArray da)
        <**> (pure (\sl d ->d { documentsignatorylinks = sl }) ))
  <*   fieldReadonlyBy "file" documentfile "Document main file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "sealed_file" documentsealedfile "Document sealed file" unjsonMaybeMainFile
  <*   fieldReadonly "author_attachments" documentauthorattachments "Author attachments"
  <*   fieldReadonly "ctime" (utcTimeToAPIFormat . documentctime) "Document creation time"
  <*   fieldReadonly "mtime" (utcTimeToAPIFormat . documentmtime) "Document modification time"
  <*   fieldReadOnlyOpt "timeout_time" (fmap utcTimeToAPIFormat . documenttimeouttime) "Document timeout time"
  <*   fieldReadOnlyOpt "auto_remind_time" (fmap utcTimeToAPIFormat . documentautoremindtime) "Document autoremind time"
  <*   fieldReadonly "status" documentstatus "Document status"
  <**> (field "days_to_sign" documentdaystosign ("Days to sign document")
        <**> (pure $ \days d -> d { documentdaystosign = days }))
  <**> (fieldOpt "days_to_remind" documentdaystoremind ("Days to remind about signing")
        <**> (pure $ \mdays d -> d { documentdaystoremind = mdays }))
  <**> (field "display_options" (documentDisplayOptions) ("Document display options")
        <**> (pure (applyDisplayOptionsToDocument)))
  <**> (field "invitation_message" (documentinvitetext) ("Document invitation text")
        <**> (pure $ \t d -> d { documentinvitetext = t }))
  <**> (field "confirmation_message" (documentconfirmtext) ("Document confirmation text")
        <**> (pure $ \t d -> d { documentconfirmtext = t }))
  <**> (field "lang" (documentlang) ("Document language")
        <**> (pure $ \l d -> d { documentlang = l }))
  <**> (fieldOpt "api_callback_url" documentapiv2callbackurl ("Document callback url (for V2)")
        <**> (pure $ \mcu d -> d { documentapiv2callbackurl = mcu }))
  <*   fieldReadonly "object_version" documentobjectversion "Document object version token"
  <*   fieldAccessToken da
  <**> (field "timezone" documenttimezonename ("Document language")
        <**> (pure $ \tz d -> d { documenttimezonename = tz }))
  <**> (field "tags" documenttags ("Document tags")
        <**> (pure $ \tags d -> d { documenttags = tags }))
  <**> (field "is_template" (isTemplate) ("Document title ")
        <**> (pure (\t d -> if t then d { documenttype = Template } else d)))
  <**> (field "is_saved" (not . documentunsaveddraft) ("Document is saved")
        <**> (pure (\t d -> if t then d { documentunsaveddraft = False } else d)))
  <*   (fieldReadonly "is_trashed" (propertyForCurrentSignatory da (isJust . signatorylinkdeleted)) ("Document is moved to trash"))
  <*   (fieldReadonly "is_deleted" (propertyForCurrentSignatory da (isJust . signatorylinkreallydeleted)) ("Document is permanently deleted"))
  <*   (fieldReadonlyBy "viewer" (\d -> Just $ viewerForDocument da d) ("Document viewer") unjsonDocumentViewer)

fieldAccessToken :: DocumentAccess -> Ap (FieldDef Document) ()
fieldAccessToken (DocumentAccess { daAccessMode }) =
  case daAccessMode of
       AuthorDocumentAccess -> accessTokenField
       AdminDocumentAccess -> accessTokenField
       _ -> pure ()
  where accessTokenField = fieldReadonly "access_token" (show . documentmagichash) "Document access token"

unjsonSignatoryArray ::  DocumentAccess -> UnjsonDef [SignatoryLink]
unjsonSignatoryArray da = arrayOf (unjsonSignatory da)

unjsonSignatory :: DocumentAccess -> UnjsonDef SignatoryLink
unjsonSignatory da =  objectOf $
       pure def
  <*   (fieldReadonly "id" signatorylinkid "Signatory id")
  <*   (fieldReadOnlyOpt "user_id"  maybesignatory "User connected to document ")
  <*   (fieldReadonly "is_author" signatoryisauthor "Is this signatory an actor")
  <**> (fieldDef "is_signatory" (signatoryispartner def) signatoryispartner "Is this signatory signing this document"
        <**> (pure $ \isa s -> s { signatoryispartner = isa }))
  <**> (fieldDefBy "fields"  (signatoryfields def) signatoryfields "Signatory field" unjsonSignatoryFields
        <**> (pure $ \fs s -> s { signatoryfields = fs }))
  <**> (fieldDef "sign_order"  (signatorysignorder def) signatorysignorder "Signatory sign order"
        <**> (pure $ \so s-> s { signatorysignorder = so }))
  <*   (fieldReadOnlyOpt "sign_time" (fmap utcTimeToAPIFormat . fmap signtime . maybesigninfo) "Time when signatory signed document")
  <*   (fieldReadOnlyOpt "seen_time" (fmap utcTimeToAPIFormat . fmap signtime . maybeseeninfo) "Time when signatory opened document in browser" )
  <*   (fieldReadOnlyOpt "read_invitation_time" (fmap utcTimeToAPIFormat . maybereadinvite) "Time when signatory read invitation" )
  <*   (fieldReadOnlyOpt "rejected_time" (fmap utcTimeToAPIFormat . signatorylinkrejectiontime) "Time when signatory rejected document" )
  <**> (fieldOpt "sign_success_redirect_url" signatorylinksignredirecturl ("Redirect user when he signed")
        <**> (pure $ \mrd s -> s { signatorylinksignredirecturl = mrd }))
  <**> (fieldOpt "reject_redirect_url" signatorylinkrejectredirecturl ("Redirect user when he rejected")
        <**> (pure $ \mrd s -> s { signatorylinkrejectredirecturl = mrd }))
  <*   (fieldReadonly "email_delivery_status" mailinvitationdeliverystatus "Email invitation delivery status")
  <*   (fieldReadonly "mobile_delivery_status" smsinvitationdeliverystatus "SMS invitation delivery status")
  <**> (fieldOpt "csv" signatorylinkcsvupload ("CSV upload for multipart") <**> (pure $ \mcsv s -> s { signatorylinkcsvupload = mcsv })) -- Check only one csv for whole doc
  <**> (fieldDef "delivery_method" (signatorylinkdeliverymethod def) signatorylinkdeliverymethod "Signatory invitation delivery method"
        <**> (pure $ \sd s -> s { signatorylinkdeliverymethod = sd }))
  <**> (fieldDef "authentication_method_to_view" (signatorylinkauthenticationtoviewmethod def) signatorylinkauthenticationtoviewmethod "Signatory authentication to view method"
        <**> (pure $ \satv s -> s { signatorylinkauthenticationtoviewmethod = satv }))
  <**> (fieldDef "authentication_method_to_sign" (signatorylinkauthenticationtosignmethod def) signatorylinkauthenticationtosignmethod "Signatory authentication to sign method"
        <**> (pure $ \sats s -> s { signatorylinkauthenticationtosignmethod = sats }))
  <**> (fieldDef "confirmation_delivery_method" (signatorylinkconfirmationdeliverymethod def) signatorylinkconfirmationdeliverymethod ""
        <**> (pure $ \scd s -> s { signatorylinkconfirmationdeliverymethod = scd }))
  <**> (fieldDef "attachments"  (signatoryattachments def) signatoryattachments "Signatory attachments"
        <**> (pure $ \sa s -> s { signatoryattachments = sa }))
  <*   fieldSignLink da

fieldSignLink :: DocumentAccess -> Ap (FieldDef SignatoryLink) ()
fieldSignLink da =
  if (canSeeSignlinks da && daStatus da /= Preparation)
    then fieldReadOnlyOpt "api_delivery_url"
          (\sl -> if (signatorylinkdeliverymethod sl == APIDelivery)
                  then (Just $ show $ LinkSignDoc (daDocumentID da) sl)
                  else Nothing
          )
          "Link for signing document"
    else pure ()


-- We can't implement lists as Unjson - since we would have to do unjsonToJSON on each document parser. And this will make us loose the order
listToJSONBS ::  (Int,[(DocumentAccess,Document)]) -> ByteString
listToJSONBS (i,docs) =
    "{\"total_matching\":"  `append` (pack $ show i) `append`
    ",\"documents\":[" `append` (docList docs) `append`
    "]}"
  where
    docList :: [(DocumentAccess,Document)] -> ByteString
    docList [] = ""
    docList l@(_:_) = $last $ scanl1 (\a b -> a `append` ",\n" `append` b) (map unjsonDocDa l)
    unjsonDocDa (da,d) = unjsonToByteStringLazy' (Options {pretty = False, indent = 0, nulls = True}) (unjsonDocument da) d
