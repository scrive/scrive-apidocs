{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V2.JSONMisc (
  utcTimeToAPIFormat
, unjsonMaybeMainFile
, textToAuthenticationToSignMethod
, evidenceAttachmentsToJSONBS
) where

import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Format
import Database.PostgreSQL.PQTypes.Binary
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T

import DB.TimeZoneName
import Data.Functor.Invariant
import Data.Unjson
import Doc.API.V2.UnjsonUtils
import Doc.DocStateData
import Doc.DocumentID
import Doc.SignatoryLinkID
import File.FileID
import KontraLink
import KontraPrelude
import User.Lang
import User.UserID
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

-- | Convert UTCTime to ISO8601 time format with two decimal places that we use
utcTimeToAPIFormat :: UTCTime -> T.Text
utcTimeToAPIFormat time = T.take 22 (T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) time)) `T.append` "Z"

instance Unjson DocumentID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromDocumentID :: DocumentID -> String) unjsonDef

instance Unjson SignatoryLinkID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromSignatoryLinkID :: SignatoryLinkID -> String) unjsonDef

instance Unjson FileID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . fromFileID :: FileID -> String) unjsonDef

instance Unjson UserID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . unUserID :: UserID -> String) unjsonDef

-- Unjson for types that are just wrappers around some base type

instance Unjson CSVUpload where
  unjsonDef = invmap (CSVUpload) csvcontents unjsonDef

instance Unjson TimeZoneName where
  unjsonDef = invmap unsafeTimeZoneName toString unjsonDef

instance Unjson SignOrder where
  unjsonDef = invmap SignOrder unSignOrder unjsonDef

instance Unjson NameOrder where
  unjsonDef = invmap NameOrder (\(NameOrder i) -> i) unjsonDef

-- Unjson for few simple enum types used

instance Unjson DocumentStatus where
  unjsonDef = unjsonEnumBy "DocumentStatus" [
      (Preparation, "preparation")
    , (Pending, "pending")
    , (Closed, "closed")
    , (Canceled, "canceled")
    , (Timedout, "timedout")
    , (Rejected, "rejected")
    , (DocumentError, "document_error")
    ]

instance Unjson DeliveryStatus where
  unjsonDef = unjsonEnumBy "DeliveryStatus" [
      (Delivered, "delivered")
    , (Undelivered, "not_delivered")
    , (Unknown, "unknown")
    , (Deferred, "deferred")
    ]

instance Unjson AuthenticationToViewMethod where
  unjsonDef = unjsonEnum "AuthenticationToViewMethod" textToAuthenticationToViewMethod authenticationToViewMethodToText

authenticationToViewMethodToText :: AuthenticationToViewMethod -> T.Text
authenticationToViewMethodToText StandardAuthenticationToView = "standard"
authenticationToViewMethodToText SEBankIDAuthenticationToView = "se_bankid"
authenticationToViewMethodToText NOBankIDAuthenticationToView = "no_bankid"

textToAuthenticationToViewMethod :: T.Text -> Maybe AuthenticationToViewMethod
textToAuthenticationToViewMethod "standard" = Just StandardAuthenticationToView
textToAuthenticationToViewMethod "se_bankid" = Just SEBankIDAuthenticationToView
textToAuthenticationToViewMethod "no_bankid" = Just NOBankIDAuthenticationToView
textToAuthenticationToViewMethod _ = Nothing

instance Unjson AuthenticationToSignMethod where
  unjsonDef = unjsonEnum "AuthenticationToSignMethod" textToAuthenticationToSignMethod authenticationToSignMethodToText

authenticationToSignMethodToText :: AuthenticationToSignMethod -> T.Text
authenticationToSignMethodToText StandardAuthenticationToSign = "standard"
authenticationToSignMethodToText SEBankIDAuthenticationToSign = "se_bankid"
authenticationToSignMethodToText SMSPinAuthenticationToSign = "sms_pin"

textToAuthenticationToSignMethod :: T.Text -> Maybe AuthenticationToSignMethod
textToAuthenticationToSignMethod "standard"  = Just StandardAuthenticationToSign
textToAuthenticationToSignMethod "se_bankid" = Just SEBankIDAuthenticationToSign
textToAuthenticationToSignMethod "sms_pin"   = Just SMSPinAuthenticationToSign
textToAuthenticationToSignMethod _           = Nothing


instance Unjson DeliveryMethod where
  unjsonDef = unjsonEnumBy "DeliveryMethod" [
      (EmailDelivery, "email")
    , (PadDelivery, "pad")
    , (APIDelivery, "api")
    , (MobileDelivery, "mobile")
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
  unjsonDef = unjsonEnum "Lang" (langFromCode . T.unpack) (T.pack . codeFromLang)

-- Unjson for simple structures used in document and signatories json

instance Unjson AuthorAttachment where
  unjsonDef = objectOf $ pure AuthorAttachment
    <*> field "id" authorattachmentfileid  "Id of file"
    <*> field "name" authorattachmentfilename "Name of file"

instance Unjson SignatoryAttachment where
  unjsonDef = objectOf $ pure (SignatoryAttachment Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <* fieldReadOnlyOpt "file" signatoryattachmentfile "Uploaded file id"

instance Unjson DocumentTag where
  unjsonDef = objectOf $ pure DocumentTag
    <*> field "name"  tagname  "Name of tag"
    <*> field "value" tagvalue "Value of tag"

-- We should not introduce instance for MainFile since this can't be really parsed. And instance for Maybe MainFile would be missleading
unjsonMaybeMainFile :: UnjsonDef (Maybe MainFile)
unjsonMaybeMainFile = nothingToNullDef $ objectOf $ pure Nothing
  <* fieldOpt "id" (fmap mainfileid)   "Id of file"
  <* fieldOpt "name" (fmap mainfilename) "Name of file"

instance Unjson Screenshot.Screenshot where
  unjsonDef = objectOf $ pure Screenshot.Screenshot
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

instance Unjson SignatoryScreenshots.SignatoryScreenshots where
  unjsonDef = objectOf $ pure combineSignatoryScreenshots
    <*> fieldOpt "first"  SignatoryScreenshots.first  "Screenshot taken when signatory looked at document"
    <*> fieldOpt "signing"  SignatoryScreenshots.signing  "Screenshot taken when signatory was about to sign"
    <*> fieldOpt "referenceName"  (join . fmap (either Just (const Nothing)) . SignatoryScreenshots.reference)  "Reference screenshot"
    <*> fieldOpt "reference"  (join .  fmap (either (const Nothing) Just) . SignatoryScreenshots.reference)  "Reference screenshot"
    where
      combineSignatoryScreenshots f s (Just rn) _ = SignatoryScreenshots.SignatoryScreenshots f s (Just $ Left rn)
      combineSignatoryScreenshots f s _ (Just rs) = SignatoryScreenshots.SignatoryScreenshots f s (Just $ Right rs)
      combineSignatoryScreenshots f s _ _ = SignatoryScreenshots.SignatoryScreenshots f s Nothing

evidenceAttachmentsToJSONBS :: DocumentID -> [EvidenceAttachments.Attachment] -> BC.ByteString
evidenceAttachmentsToJSONBS did eas = "[" `BC.append` (BC.intercalate "," $ map eatobs (sortBy eaSorter eas)) `BC.append` "]"
  where eatobs :: EvidenceAttachments.Attachment -> BC.ByteString
        eatobs ea = "{\"name\":\"" `BC.append` (BC.fromStrict $ EvidenceAttachments.name ea) `BC.append` "\"" `BC.append`
                    (case EvidenceAttachments.mimetype ea of
                          Just mt -> ",\"mimetype\":\"" `BC.append` (BC.fromStrict mt) `BC.append` "\""
                          Nothing -> ""
                    ) `BC.append`
                    ",\"download_url\":\"" `BC.append` (BC.pack $ show $ LinkEvidenceAttachment did (EvidenceAttachments.name ea)) `BC.append` "\"" `BC.append`
                    "}"
        eaSorter :: EvidenceAttachments.Attachment -> EvidenceAttachments.Attachment -> Ordering
        eaSorter a b | EvidenceAttachments.name a == firstAttachmentName = LT
                     | EvidenceAttachments.name b == firstAttachmentName = GT
                     | otherwise = compare a b
        firstAttachmentName = "Evidence Quality of Scrive E-signed Documents.html"
