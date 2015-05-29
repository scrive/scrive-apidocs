{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSONMisc (unjsonMaybeMainFile,textToAuthenticationMethod) where

import Data.Text.Encoding
import Database.PostgreSQL.PQTypes.Binary
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.Aeson as Aeson

import Doc.DocStateData
import KontraPrelude
import File.FileID
import Data.Unjson
import Doc.DocumentID
import Data.Text
import User.Lang
import DB.TimeZoneName
import Data.Functor.Invariant
import Doc.SignatoryLinkID
import User.UserID
import Doc.API.V2.UnjsonUtils
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

import qualified Data.ByteString.UTF8 as BS

-- Unjson for various ID types

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
  unjsonDef = unjsonEnum "AuthenticationMethod" textToAuthenticationMethod authenticationMethodToText

authenticationMethodToText :: AuthenticationMethod -> Text
authenticationMethodToText StandardAuthentication = "standard"
authenticationMethodToText ELegAuthentication = "sv_bankid"
authenticationMethodToText SMSPinAuthentication = "sms_pin"

textToAuthenticationMethod :: Text -> Maybe AuthenticationMethod
textToAuthenticationMethod "standard"  = Just StandardAuthentication
textToAuthenticationMethod "sv_bankid" = Just ELegAuthentication
textToAuthenticationMethod "sms_pin"   = Just SMSPinAuthentication
textToAuthenticationMethod _           = Nothing


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

-- Unjson for simple structures used in document and signatories json

instance Unjson AuthorAttachment where
  unjsonDef = objectOf $ pure AuthorAttachment
    <*> field "id" authorattachmentfileid  "Id of file"
    <*> field "name" authorattachmentfilename "Name of file"

instance Unjson SignatoryAttachment where
  unjsonDef = objectOf $ pure (SignatoryAttachment Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <* fieldReadonlyBy "file" signatoryattachmentfile "Uploaded file id" unjsonDefWithNull

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


{-
  toJSValue (Screenshot time' (Binary image')) = runJSONGen $ do
    value "time"  $ toJSValue $ formatTimeISO time'
    value "image" $ toJSValue $ BS.toString $ RFC2397.encode "image/jpeg" image'
    -}