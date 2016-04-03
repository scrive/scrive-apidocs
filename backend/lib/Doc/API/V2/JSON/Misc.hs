{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V2.JSON.Misc (
  utcTimeToAPIFormat
, unjsonDocumentStatus
, unjsonDeliveryStatus
, unjsonAuthenticationToViewMethod
, unjsonAuthenticationToSignMethod
, unjsonDeliveryMethod
, unjsonConfirmationDeliveryMethod
, unjsonLang
, unjsonAuthorAttachment
, unjsonSignatoryAttachment
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
import Database.PostgreSQL.PQTypes.Binary
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.DocumentID
import KontraLink
import KontraPrelude
import User.Lang
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

-- | Convert UTCTime to ISO8601 time format with two decimal places that we use
utcTimeToAPIFormat :: UTCTime -> T.Text
utcTimeToAPIFormat time = T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) time) <> "Z"

-- Unjson for few simple enum types used
unjsonDocumentStatus :: UnjsonDef DocumentStatus
unjsonDocumentStatus = unjsonEnumBy "DocumentStatus" [
      (Preparation, "preparation")
    , (Pending, "pending")
    , (Closed, "closed")
    , (Canceled, "canceled")
    , (Timedout, "timedout")
    , (Rejected, "rejected")
    , (DocumentError, "document_error")
    ]

unjsonDeliveryStatus :: UnjsonDef DeliveryStatus
unjsonDeliveryStatus = unjsonEnumBy "DeliveryStatus" [
      (Delivered, "delivered")
    , (Undelivered, "not_delivered")
    , (Unknown, "unknown")
    , (Deferred, "deferred")
    ]

unjsonAuthenticationToViewMethod :: UnjsonDef AuthenticationToViewMethod
unjsonAuthenticationToViewMethod = unjsonEnumBy "AuthenticationToViewMethod" [
      (StandardAuthenticationToView, "standard")
    , (SEBankIDAuthenticationToView, "se_bankid")
    , (NOBankIDAuthenticationToView, "no_bankid")
    ]

unjsonAuthenticationToSignMethod :: UnjsonDef AuthenticationToSignMethod
unjsonAuthenticationToSignMethod = unjsonEnumBy "AuthenticationToSignMethod" [
      (StandardAuthenticationToSign, "standard")
    , (SEBankIDAuthenticationToSign, "se_bankid")
    , (SMSPinAuthenticationToSign, "sms_pin")
    ]

unjsonDeliveryMethod :: UnjsonDef DeliveryMethod
unjsonDeliveryMethod = unjsonEnumBy "DeliveryMethod" [
      (EmailDelivery, "email")
    , (PadDelivery, "pad")
    , (APIDelivery, "api")
    , (MobileDelivery, "mobile")
    , (EmailAndMobileDelivery, "email_mobile")
    ]

unjsonConfirmationDeliveryMethod :: UnjsonDef ConfirmationDeliveryMethod
unjsonConfirmationDeliveryMethod = unjsonEnumBy "ConfirmationDeliveryMethod" [
      (EmailConfirmationDelivery, "email")
    , (MobileConfirmationDelivery, "mobile")
    , (EmailAndMobileConfirmationDelivery, "email_mobile")
    , (NoConfirmationDelivery, "none")
    ]

unjsonLang :: UnjsonDef Lang
unjsonLang = unjsonEnum "Lang" (langFromCode . T.unpack) (T.pack . codeFromLang)

-- Unjson for simple structures used in document and signatories json

unjsonAuthorAttachment :: UnjsonDef AuthorAttachment
unjsonAuthorAttachment = objectOf $ pure AuthorAttachment
    <*> field "name" authorattachmentname "Name of file"
    <*> field "required" authorattachmentrequired "Are signatories required to accept attachment before signing"
    <*> field "file_id" authorattachmentfileid  "Id of file"

unjsonSignatoryAttachment :: UnjsonDef SignatoryAttachment
unjsonSignatoryAttachment = objectOf $ pure (SignatoryAttachment Nothing Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <* fieldReadOnlyOpt "file_id" signatoryattachmentfile "Uploaded file id"
    <* fieldReadOnlyOpt "file_name" signatoryattachmentfilename "Uploaded filename"

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
    <*> fieldBy "image" Screenshot.image "Image with screenshot, base64 encoded with content type" (invmap Binary unBinary unjsonImage)
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

evidenceAttachmentsToJSONBS :: DocumentID -> [EvidenceAttachments.Attachment] -> BSC.ByteString
evidenceAttachmentsToJSONBS did eas = toLazyByteString $ "{ \"attachments\": ["  <> (eaList (sortBy eaSorter eas)) <> "]}"
  where
    eaList :: [EvidenceAttachments.Attachment] -> Builder
    eaList [] = ""
    eaList [l] = (eaJSON l)
    eaList (l:ls) = (eaJSON l) <> "," <> eaList ls

    eaJSON :: EvidenceAttachments.Attachment -> Builder
    eaJSON ea = "{\"name\":\"" <> (lazyByteString $ BSC.fromStrict $ EvidenceAttachments.name ea) <> "\"" <>
                    (case EvidenceAttachments.mimetype ea of
                          Just mt -> ",\"mimetype\":\"" <> (lazyByteString $ BSC.fromStrict mt) <> "\""
                          Nothing -> ""
                    ) <>
                    ",\"download_url\":\"" <> (lazyByteString $ BSC.pack $ show $ LinkEvidenceAttachment did (EvidenceAttachments.name ea))<>  "\"" <>
                    "}"

    eaSorter :: EvidenceAttachments.Attachment -> EvidenceAttachments.Attachment -> Ordering
    eaSorter a b | EvidenceAttachments.name a == firstAttachmentName = LT
                 | EvidenceAttachments.name b == firstAttachmentName = GT
                 | otherwise = compare a b
    firstAttachmentName = "Evidence Quality of Scrive E-signed Documents.html"

unjsonCSVUpload :: UnjsonDef CSVUpload
unjsonCSVUpload = invmap CSVUpload csvcontents unjsonDef
