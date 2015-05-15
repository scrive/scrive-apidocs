{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSON (unjsonDocument) where


import Doc.DocStateData
import KontraPrelude
import Utils.Default
import Utils.Read
import File.FileID
import Data.Unjson
import Doc.DocumentID
import Data.Text
import qualified Data.Aeson as Aeson
import Doc.DocUtils
import User.Lang
import DB.TimeZoneName
import Data.Functor.Invariant
import Doc.SignatoryLinkID
import User.UserID

unjsonDocument :: UnjsonDef Document
unjsonDocument = objectOf $
       pure defaultValue
  <*   fieldReadonly "id" (documentid) "Document id "
  <**> (field "title" documenttitle ("Document title")
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "signatories" (documentsignatorylinks) ("Document signatories") unjsonSignatoryArray
        <**> (pure (\sl d ->d { documentsignatorylinks = sl }) ))
  <*   fieldReadonlyBy "file" documentfile "Document main file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "sealed_file" documentsealedfile "Document sealed file" unjsonMaybeMainFile
  <**> (field "author_attachments" documentauthorattachments "Author attachments"
        <**> (pure $ \atts d -> d { documentauthorattachments = atts }))
  <*   fieldReadonly "ctime" documentctime "Document creation time"
  <*   fieldReadonly "mtime" documentmtime "Document modification time"
  <*   fieldReadonlyBy "timeout_time" documenttimeouttime "Document timeout time" unjsonDefWithNull
  <*   fieldReadonlyBy "auto_remind_time" documentautoremindtime "Document autoremind time" unjsonDefWithNull
  <*   fieldReadonly "status" documentstatus "Document status"
  <**> (field "days_to_sign" documentdaystosign ("Days to sign document")
        <**> (pure $ \days d -> d { documentdaystosign = days }))
  <**> (fieldOpt "days_to_remind" documentdaystoremind ("Days to remind about signing")
        <**> (pure $ \mdays d -> d { documentdaystoremind = mdays }))
  <**> (fieldBy "display_options" (documentDisplayOptions) ("Document display options") unjsonDocumentDisplayOptions
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


unjsonSignatoryArray :: UnjsonDef [SignatoryLink]
unjsonSignatoryArray = arrayWithPrimaryKeyOf (signatorylinkid) (objectOf $ fieldDef "id" (unsafeSignatoryLinkID 0) id "Signatory ID") unjsonSignatory

unjsonSignatory :: UnjsonDef SignatoryLink
unjsonSignatory =  objectOf $
       pure defaultValue
  <**> (fieldDef "id" (signatorylinkid defaultValue) signatorylinkid "Signatory id"
        <**> (pure $ \sid s -> s { signatorylinkid = sid })) -- Feels like readonly - but it's not true - since we need to keep matching
  <*   (fieldReadonlyBy "user_id"  maybesignatory "User connected to document " unjsonDefWithNull)
  <**> (fieldDef "is_author" (signatoryisauthor defaultValue) signatoryisauthor "Is this signatory an actor"
        <**> (pure $ \isa s -> s { signatoryisauthor = isa }))
  <**> (fieldDef "is_signatory" (signatoryispartner defaultValue) signatoryispartner "Is this signatory signing this document"
        <**> (pure $ \isa s -> s { signatoryispartner = isa }))
  <**> (fieldDefBy "fields"  (signatoryfields defaultValue) signatoryfields "Signatory field" (objectOf $ pure [])
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




data DocumentDisplayOptions = DocumentDisplayOptions {
    showHeader :: Bool
  , showPdfDownload :: Bool
  , showReject :: Bool
  , showFooter :: Bool
}

documentDisplayOptions :: Document -> DocumentDisplayOptions
documentDisplayOptions doc = DocumentDisplayOptions {
      showHeader = documentshowheader doc
    , showPdfDownload = documentshowpdfdownload doc
    , showReject = documentshowrejectoption doc
    , showFooter = documentshowfooter doc
  }

applyDisplayOptionsToDocument :: DocumentDisplayOptions -> Document -> Document
applyDisplayOptionsToDocument displayOptions doc = doc {
      documentshowheader = showHeader displayOptions
    , documentshowpdfdownload = showPdfDownload displayOptions
    , documentshowrejectoption = showReject displayOptions
    , documentshowfooter = showFooter displayOptions
  }

unjsonDocumentDisplayOptions :: UnjsonDef DocumentDisplayOptions
unjsonDocumentDisplayOptions = objectOf $ pure DocumentDisplayOptions
  <*> field "show_header" showHeader "Show header while signing"
  <*> field "show_pdf_download" showPdfDownload "Show download option while signing"
  <*> field "show_reject_option" showReject "Show signatories option to reject document"
  <*> field "show_footer" showFooter "Show footer while signing"


unjsonEnumBy :: (Eq a) => Text -> [(a,Text)] -> UnjsonDef a
unjsonEnumBy desc enumDef =
  unjsonEnum desc (parseEnum enumDef) (printEnum enumDef)
  where

    parseEnum :: [(a,Text)] -> Text -> Maybe a
    parseEnum ((m,t):ms) v = if (t == v)
                            then Just m
                            else parseEnum ms v
    parseEnum _ _ = Nothing

    printEnum :: (Eq a) => [(a,Text)] -> a -> Text
    printEnum ((m,t):ms) a =  if (a == m)
                            then t
                            else printEnum ms a
    printEnum _ _ = $unexpectedError $ unpack ("Incompleate printEnum definition for " `append` desc)


unjsonEnum :: Text -> (Text -> Maybe a) -> (a -> Text) -> UnjsonDef a
unjsonEnum desc parseEnum printEnum  =
  SimpleUnjsonDef desc parseFromEnumFromAeson printEnumToAeson
  where
    printEnumToAeson a = Aeson.String $ printEnum a
    parseFromEnumFromAeson (Aeson.String s) = case (parseEnum s) of
                                                Just a -> Result a []
                                                _ -> fail $ unpack $ "cannot parse enum " `append` desc `append` " from " `append` s
    parseFromEnumFromAeson _ = fail $ unpack $ "cannot parse enum " `append` desc `append` " from not string"

instance Unjson DocumentStatus where
  unjsonDef = unjsonEnumBy "DocumentStatus" [
      (Preparation, "Preparation")
    , (Pending, "Pending")
    , (Closed, "Closed")
    , (Canceled, "Canceled")
    , (Timedout, "Timedout")
    , (Rejected, "Rejected")
    , (DocumentError, "DocumentError")
    ]

instance Unjson DeliveryStatus where
  unjsonDef = unjsonEnumBy "DeliveryStatus" [
      (Delivered, "delivered")
    , (Undelivered, "not_delivered")
    , (Unknown, "unknown")
    , (Deferred, "deferred")
    ]

instance Unjson AuthenticationMethod where
  unjsonDef = unjsonEnumBy "AuthenticationMethod" [
      (StandardAuthentication, "standard")
    , (ELegAuthentication, "sv_bankid")
    , (SMSPinAuthentication, "sms_pin")
    ]

instance Unjson DeliveryMethod where
  unjsonDef = unjsonEnumBy "DeliveryMethod" [
      (EmailDelivery, "email")
    , (PadDelivery, "pad")
    , (APIDelivery, "unknown")
    , (MobileDelivery, "api")
    , (EmailAndMobileDelivery, "email_mobile")
    ]

instance Unjson ConfirmationDeliveryMethod where
  unjsonDef = unjsonEnumBy "ConfirmationDeliveryMethod" [
      (EmailConfirmationDelivery, "email")
    , (MobileConfirmationDelivery, "mobile")
    , (EmailAndMobileConfirmationDelivery, "email_mobile")
    , (NoConfirmationDelivery, "none")
    ]

instance Unjson Lang where
  unjsonDef = unjsonEnum "Lang" (langFromCode . unpack) (pack . codeFromLang)

unjsonMaybeMainFile :: UnjsonDef (Maybe MainFile)
unjsonMaybeMainFile = nothingToNullDef $ objectOf $ pure Nothing
  <* fieldOpt "id" (fmap mainfileid)   "Id of file"
  <* fieldOpt "name" (fmap mainfilename) "Name of file"


instance Unjson CSVUpload where
  unjsonDef = unjsonCSVUpload

unjsonCSVUpload :: UnjsonDef CSVUpload
unjsonCSVUpload = invmap (CSVUpload "") csvcontents unjsonDef

instance Unjson AuthorAttachment where
  unjsonDef = unjsonAuthorAttachments

unjsonAuthorAttachments :: UnjsonDef AuthorAttachment
unjsonAuthorAttachments = objectOf $ pure AuthorAttachment
    <*> field "id" authorattachmentfileid  "Id of file"
    <*> field "name" authorattachmentfilename "Name of file"

instance Unjson SignatoryAttachment where
  unjsonDef = unjsonSignatoryAttachment

unjsonSignatoryAttachment :: UnjsonDef SignatoryAttachment
unjsonSignatoryAttachment = objectOf $ pure (SignatoryAttachment Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <* fieldReadonlyBy "file" signatoryattachmentfile "Uploaded file id" unjsonDefWithNull

instance Unjson DocumentTag where
  unjsonDef = unjsonDocumentTag

unjsonDocumentTag :: UnjsonDef DocumentTag
unjsonDocumentTag = objectOf $ pure DocumentTag
    <*> field "name"  tagname  "Name of tag"
    <*> field "value" tagvalue "Value of tag"

unjsonDocumentID :: UnjsonDef DocumentID
unjsonDocumentID = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromDocumentID :: DocumentID -> String) unjsonDef

instance Unjson DocumentID where
  unjsonDef = unjsonDocumentID

unjsonSignatoryLinkID :: UnjsonDef SignatoryLinkID
unjsonSignatoryLinkID = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromSignatoryLinkID :: SignatoryLinkID -> String) unjsonDef

instance Unjson SignatoryLinkID where
  unjsonDef = unjsonSignatoryLinkID

unjsonFileID :: UnjsonDef FileID
unjsonFileID = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . fromFileID :: FileID -> String) unjsonDef

instance Unjson FileID where
  unjsonDef = unjsonFileID

unjsonUserID :: UnjsonDef UserID
unjsonUserID = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . unUserID :: UserID -> String) unjsonDef

instance Unjson UserID where
  unjsonDef = unjsonUserID

unjsonTimeZoneName :: UnjsonDef TimeZoneName
unjsonTimeZoneName = invmap unsafeTimeZoneName toString unjsonDef

instance Unjson TimeZoneName where
  unjsonDef = unjsonTimeZoneName

instance Unjson SignOrder where
  unjsonDef = unjsonSignOrder

unjsonSignOrder :: UnjsonDef SignOrder
unjsonSignOrder = invmap SignOrder unSignOrder unjsonDef

-- Utils
nothingToNullDef :: UnjsonDef (Maybe a) ->  UnjsonDef (Maybe a)
nothingToNullDef def = SimpleUnjsonDef "ReadOnly"  (parse def) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    _ -> unjsonToJSON def mv

unjsonDefWithNull :: Unjson a => UnjsonDef (Maybe a)
unjsonDefWithNull = SimpleUnjsonDef "ReadOnly"  (\v -> Just <$> parse unjsonDef v) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    Just v -> unjsonToJSON unjsonDef v

