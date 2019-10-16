{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V2.JSON.Misc (
  utcTimeToAPIFormat
, unjsonDocumentStatus
, unjsonSignatoryRole
, unjsonDeliveryStatus
, unjsonAuthenticationToViewMethod
, unjsonAuthenticationToSignMethod
, unjsonDeliveryMethod
, unjsonConfirmationDeliveryMethod
, unjsonNotificationDeliveryMethod
, unjsonLang
, unjsonAuthorAttachment
, unjsonSignatoryAttachment
, unjsonHighlightedPage
, unjsonDocumentTag
, unjsonMaybeMainFile
, unjsonSignatoryScreenshots
, unjsonCSVUpload
, evidenceAttachmentsToJSONBS
) where

import Data.ByteString.Builder
import Data.Functor.Invariant
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Format
import Data.Unjson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.DocumentID
import KontraLink
import User.Lang
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

-- | Convert UTCTime to ISO8601 time format with two decimal places that we use
utcTimeToAPIFormat :: UTCTime -> Text
utcTimeToAPIFormat time = T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%5Q")) time) <> "Z"

-- Unjson for few simple enum types used
unjsonDocumentStatus :: UnjsonDef DocumentStatus
unjsonDocumentStatus = unjsonEnumBy "DocumentStatus" [
      (Preparation,   "preparation")
    , (Pending,       "pending")
    , (Closed,        "closed")
    , (Canceled,      "canceled")
    , (Timedout,      "timedout")
    , (Rejected,      "rejected")
    , (DocumentError, "document_error")
    ]

unjsonSignatoryRole :: UnjsonDef SignatoryRole
unjsonSignatoryRole = unjsonEnumBy "SignatoryRole"
    [ (SignatoryRoleViewer,         "viewer")
    , (SignatoryRoleApprover,       "approver")
    , (SignatoryRoleSigningParty,   "signing_party")
    , (SignatoryRoleForwardedSigningParty, "forwarded_party")
    , (SignatoryRoleForwardedApprover,  "forwarded_party")
    ]

unjsonDeliveryStatus :: UnjsonDef DeliveryStatus
unjsonDeliveryStatus = unjsonEnumBy "DeliveryStatus" [
      (Delivered,   "delivered")
    , (Undelivered, "not_delivered")
    , (Unknown,     "unknown")
    , (Deferred,    "deferred")
    ]

unjsonAuthenticationToViewMethod :: UnjsonDef AuthenticationToViewMethod
unjsonAuthenticationToViewMethod = unjsonEnumBy "AuthenticationToViewMethod" [
      (StandardAuthenticationToView, "standard")
    , (SEBankIDAuthenticationToView, "se_bankid")
    , (NOBankIDAuthenticationToView, "no_bankid")
    , (DKNemIDAuthenticationToView,  "dk_nemid")
    , (FITupasAuthenticationToView,  "fi_tupas")
    , (SMSPinAuthenticationToView,   "sms_pin")
    , (VerimiAuthenticationToView,   "verimi")
    , (IDINAuthenticationToView,     "nl_idin")
    ]

unjsonAuthenticationToSignMethod :: UnjsonDef AuthenticationToSignMethod
unjsonAuthenticationToSignMethod = unjsonEnumBy "AuthenticationToSignMethod" [
      (StandardAuthenticationToSign, "standard")
    , (SEBankIDAuthenticationToSign, "se_bankid")
    , (NOBankIDAuthenticationToSign, "no_bankid")
    , (DKNemIDAuthenticationToSign,  "dk_nemid")
    , (SMSPinAuthenticationToSign,   "sms_pin")
    ]

unjsonDeliveryMethod :: UnjsonDef DeliveryMethod
unjsonDeliveryMethod = unjsonEnumBy "DeliveryMethod" [
      (EmailDelivery,          "email")
    , (PadDelivery,            "pad")
    , (APIDelivery,            "api")
    , (MobileDelivery,         "mobile")
    , (EmailAndMobileDelivery, "email_mobile")
    , (PortalDelivery,         "portal")
    ]

unjsonConfirmationDeliveryMethod :: UnjsonDef ConfirmationDeliveryMethod
unjsonConfirmationDeliveryMethod = unjsonEnumBy "ConfirmationDeliveryMethod" [
      (EmailConfirmationDelivery,              "email")
    , (EmailLinkConfirmationDelivery,          "email_link")
    , (MobileConfirmationDelivery,             "mobile")
    , (EmailAndMobileConfirmationDelivery,     "email_mobile")
    , (EmailLinkAndMobileConfirmationDelivery, "email_link_mobile")
    , (NoConfirmationDelivery,                 "none")
    ]

unjsonNotificationDeliveryMethod :: UnjsonDef NotificationDeliveryMethod
unjsonNotificationDeliveryMethod = unjsonEnumBy "NotificationDeliveryMethod" [
      (NoNotificationDelivery,                 "none")
    , (EmailNotificationDelivery,              "email")
    , (MobileNotificationDelivery,             "mobile")
    , (EmailAndMobileNotificationDelivery,     "email_mobile")
    ]


unjsonLang :: UnjsonDef Lang
unjsonLang = unjsonEnum "Lang" langFromCode codeFromLang

-- Unjson for simple structures used in document and signatories json

unjsonAuthorAttachment :: UnjsonDef AuthorAttachment
unjsonAuthorAttachment = objectOf $ pure AuthorAttachment
    <*> field "name" authorattachmentname "Name of file"
    <*> field "required" authorattachmentrequired "Are signatories required to accept attachment before signing"
    <*> field "add_to_sealed_file" authorattachmentaddtosealedfile "Should attachmnet be added to sealed file"
    <*> field "file_id" authorattachmentfileid  "Id of file"

unjsonSignatoryAttachment :: UnjsonDef SignatoryAttachment
unjsonSignatoryAttachment = objectOf $ pure (SignatoryAttachment Nothing Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <*> fieldDef "required" True signatoryattachmentrequired "Uploading attachment is mandatory"
    <* fieldReadOnlyOpt "file_id" signatoryattachmentfile "Uploaded file id"
    <* fieldReadOnlyOpt "file_name" signatoryattachmentfilename "Uploaded filename"

unjsonHighlightedPage :: UnjsonDef HighlightedPage
unjsonHighlightedPage = objectOf $ pure HighlightedPage
    <*> field "page"  highlightedPagePage  "Page that was highlighted"
    <*> field "file_id" highlightedPageFileID "File ID of image with highlighting"

unjsonDocumentTag :: UnjsonDef DocumentTag
unjsonDocumentTag = objectOf $ pure DocumentTag
    <*> field "name"  tagname  "Name of tag"
    <*> field "value" tagvalue "Value of tag"

-- We should not introduce instance for MainFile since this can't be really parsed. And instance for Maybe MainFile would be missleading
unjsonMaybeMainFile :: UnjsonDef (Maybe MainFile)
unjsonMaybeMainFile = nothingToNullDef $ objectOf $ pure Nothing
  <* fieldOpt "id" (fmap mainfileid)   "Id of file"
  <* fieldOpt "name" (fmap mainfilename) "Name of file"

unjsonScreenshots :: UnjsonDef Screenshot.Screenshot
unjsonScreenshots = objectOf $ pure Screenshot.Screenshot
    <*> field "time"  Screenshot.time  "Time when screenshot was taken"
    <*> fieldBy "image" Screenshot.image "Image with screenshot, base64 encoded with content type" unjsonImage
    where
      unjsonImage :: UnjsonDef BS.ByteString
      unjsonImage = SimpleUnjsonDef "Screenshot" parseScreenshot (Aeson.String . decodeUtf8 . (RFC2397.encode "image/jpeg"))
      parseScreenshot :: Aeson.Value -> Result BS.ByteString
      parseScreenshot (Aeson.String t ) = case RFC2397.decode $ encodeUtf8 t of
                                            Just (_,v) -> pure v
                                            _ -> fail "Can't parse screenshot image encoded string. RFC2397 encoding expected"
      parseScreenshot _ = fail "Can't parse screenshot image from something that is not string"

unjsonSignatoryScreenshots :: UnjsonDef SignatoryScreenshots.SignatoryScreenshots
unjsonSignatoryScreenshots = objectOf $ pure combineSignatoryScreenshots
    <*> fieldOptBy "first"  SignatoryScreenshots.first  "Screenshot taken when signatory looked at document" unjsonScreenshots
    <*> fieldOptBy "signing"  SignatoryScreenshots.signing  "Screenshot taken when signatory was about to sign" unjsonScreenshots
    <*> fieldOpt "referenceName"  (join . fmap (either Just (const Nothing)) . SignatoryScreenshots.reference)  "Reference screenshot"
    <*> fieldOptBy "reference"  (join .  fmap (either (const Nothing) Just) . SignatoryScreenshots.reference)  "Reference screenshot" unjsonScreenshots
    where
      combineSignatoryScreenshots f s (Just rn) _ = SignatoryScreenshots.SignatoryScreenshots f s (Just $ Left rn)
      combineSignatoryScreenshots f s _ (Just rs) = SignatoryScreenshots.SignatoryScreenshots f s (Just $ Right rs)
      combineSignatoryScreenshots f s _ _ = SignatoryScreenshots.SignatoryScreenshots f s Nothing

evidenceAttachmentsToJSONBS :: DocumentID -> [Text] -> BSC.ByteString
evidenceAttachmentsToJSONBS did eas = toLazyByteString $ "{ \"attachments\": ["  <> (eaList (sortBy eaSorter eas)) <> "]}"
  where
    eaList :: [Text] -> Builder
    eaList [] = ""
    eaList [l] = (eaJSON l)
    eaList (l:ls) = (eaJSON l) <> "," <> eaList ls

    eaJSON :: Text -> Builder
    eaJSON name = "{\"name\":\"" <> (lazyByteString $ BSC.fromString $ T.unpack $ name) <> "\"" <>
                  ",\"download_url\":\"" <>
                  (lazyByteString $ BSC.fromString $ show $ LinkEvidenceAttachment did name) <>
                  "\"" <>
                  "}"

    eaSorter :: Text -> Text -> Ordering
    eaSorter a b | a == firstAttachmentName = LT
                 | b == firstAttachmentName = GT
                 | otherwise = compare a b
    firstAttachmentName = "Evidence Quality of Scrive E-signed Documents.html"

unjsonCSVUpload :: UnjsonDef CSVUpload
unjsonCSVUpload = invmap CSVUpload csvcontents unjsonDef
