{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSONDocument (unjsonDocument,listToJSONBS) where

import Control.Applicative.Free
import Doc.DocStateData
import KontraPrelude
import Utils.Default
import Data.Unjson
import Doc.DocUtils
import Doc.API.V2.UnjsonUtils
import Doc.API.V2.JSONMisc
import Doc.API.V2.JSONFields
import Doc.API.V2.DisplayOptions
import Doc.API.V2.DocumentAccess
import Doc.API.V2.DocumentViewer
import KontraLink
import Data.ByteString.Lazy.Char8

unjsonDocument :: DocumentAccess -> UnjsonDef Document
unjsonDocument da = objectOf $
       pure defaultValue
  <*   fieldReadonly "id" (documentid) "Document id "
  <**> (field "title" documenttitle ("Document title")
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "signatories" (documentsignatorylinks) ("Document signatories") (unjsonSignatoryArray da)
        <**> (pure (\sl d ->d { documentsignatorylinks = sl }) ))
  <*   fieldReadonlyBy "file" documentfile "Document main file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "sealed_file" documentsealedfile "Document sealed file" unjsonMaybeMainFile
  <*   fieldReadonly "author_attachments" documentauthorattachments "Author attachments"
  <*   fieldReadonly "ctime" documentctime "Document creation time"
  <*   fieldReadonly "mtime" documentmtime "Document modification time"
  <*   fieldReadonlyBy "timeout_time" documenttimeouttime "Document timeout time" unjsonDefWithNull
  <*   fieldReadonlyBy "auto_remind_time" documentautoremindtime "Document autoremind time" unjsonDefWithNull
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
  <**> (fieldOpt "api_callback_url" documentapicallbackurl ("Document language")
        <**> (pure $ \mcu d -> d { documentapicallbackurl = mcu }))
  <*   fieldReadonly "object_version" documentobjectversion "Document object version token"
  <*   fieldReadonly "access_token" (show . documentmagichash)   "Document access token"
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


unjsonSignatoryArray ::  DocumentAccess -> UnjsonDef [SignatoryLink]
unjsonSignatoryArray da = arrayOf (unjsonSignatory da)

unjsonSignatory :: DocumentAccess -> UnjsonDef SignatoryLink
unjsonSignatory da =  objectOf $
       pure defaultValue
  <*   (fieldReadonly "id" signatorylinkid "Signatory id")
  <*   (fieldReadonlyBy "user_id"  maybesignatory "User connected to document " unjsonDefWithNull)
  <*   (fieldReadonly "is_author" signatoryisauthor "Is this signatory an actor")
  <**> (fieldDef "is_signatory" (signatoryispartner defaultValue) signatoryispartner "Is this signatory signing this document"
        <**> (pure $ \isa s -> s { signatoryispartner = isa }))
  <**> (fieldDefBy "fields"  (signatoryfields defaultValue) signatoryfields "Signatory field" unjsonSignatoryFields
        <**> (pure $ \fs s -> s { signatoryfields = fs }))
  <**> (fieldDef "sign_order"  (signatorysignorder defaultValue) signatorysignorder "Signatory sign order"
        <**> (pure $ \so s-> s { signatorysignorder = so }))
  <*   (fieldReadonlyBy "sign_time" (fmap signtime . maybesigninfo) "Time when signatory signed document" unjsonDefWithNull)
  <*   (fieldReadonlyBy "seen_time" (fmap signtime . maybeseeninfo) "Time when signatory opened document in browser" unjsonDefWithNull)
  <*   (fieldReadonlyBy "read_invitation_time" maybereadinvite "Time when signatory read invitation" unjsonDefWithNull)
  <*   (fieldReadonlyBy "rejected_time" signatorylinkrejectiontime "Time when signatory rejected document" unjsonDefWithNull)
  <**> (fieldOpt "sign_success_redirect_url" signatorylinksignredirecturl ("Redirect user when he signed")
        <**> (pure $ \mrd s -> s { signatorylinksignredirecturl = mrd }))
  <**> (fieldOpt "reject_redirect_url" signatorylinkrejectredirecturl ("Redirect user when he rejected")
        <**> (pure $ \mrd s -> s { signatorylinkrejectredirecturl = mrd }))
  <*   (fieldReadonly "email_delivery_status" mailinvitationdeliverystatus "Email invitation delivery status")
  <*   (fieldReadonly "mobile_delivery_status" mailinvitationdeliverystatus "SMS invitation delivery status")
  <**> (fieldOpt "csv" signatorylinkcsvupload ("CSV upload for multipart") <**> (pure $ \mcsv s -> s { signatorylinkcsvupload = mcsv })) -- Check only one csv for whole doc
  <**> (fieldDef "delivery_method" (signatorylinkdeliverymethod defaultValue) signatorylinkdeliverymethod "Signatory invitation delivery method"
        <**> (pure $ \sd s -> s { signatorylinkdeliverymethod = sd }))
  <**> (fieldDef "authentication_method" (signatorylinkauthenticationmethod defaultValue) signatorylinkauthenticationmethod "Signatory authentication method"
        <**> (pure $ \sa s -> s { signatorylinkauthenticationmethod = sa }))
  <**> (fieldDef "confirmation_delivery_method" (signatorylinkconfirmationdeliverymethod defaultValue) signatorylinkconfirmationdeliverymethod "Signatory authentication method"
        <**> (pure $ \scd s -> s { signatorylinkconfirmationdeliverymethod = scd }))
  <**> (fieldDef "attachments"  (signatoryattachments defaultValue) signatoryattachments "Signatory attachments"
        <**> (pure $ \sa s -> s { signatoryattachments = sa }))
  <*   fieldSignLink da

fieldSignLink :: DocumentAccess -> Ap (FieldDef SignatoryLink) ()
fieldSignLink da =
  if (canSeeSignlinks da)
    then fieldReadonlyBy "signing_link"
          (\sl -> if (signatorylinkdeliverymethod sl == APIDelivery)
                  then (Just $ show $ LinkSignDoc (daDocumentID da) sl)
                  else Nothing
          )
          "Link for signing document" unjsonDefWithNull
    else pure ()


-- We can't implement lists as Unjson - since we would have to do unjsonToJSON on each document parser. And this will make us loose the order
listToJSONBS ::  (Int,[(DocumentAccess,Document)]) -> ByteString
listToJSONBS (i,docs) =
    "{\n" `append`
    "  \"more\": " `append` (pack $ show i) `append` ",\n" `append`
    "  \"list\": [\n"  `append` (docList docs) `append`
    "\n  ]"  `append`
    "}"

  where
    docList :: [(DocumentAccess,Document)] -> ByteString
    docList ((a,d):(l:ls)) = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) (unjsonDocument a) d `append` ",\n" `append` docList (l:ls)
    docList ((a,d):[]) = unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) (unjsonDocument a) d
    docList [] = ""
