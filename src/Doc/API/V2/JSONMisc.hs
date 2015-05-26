{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSONMisc (unjsonMaybeMainFile) where


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

