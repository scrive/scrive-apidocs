{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Data.SignatoryLink (
    SignOrder(..)
  , SignInfo(..)
  , DeliveryStatus(..)
  , CSVUpload(..)
  , AuthenticationMethod(..)
  , DeliveryMethod(..)
  , ConfirmationDeliveryMethod(..)
  , SignatoryLink(..)
  , signatoryLinksSelectors
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Int
import Data.Monoid
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

import DB.Derive
import Doc.Data.SignatoryAttachment
import Doc.Data.SignatoryField
import Doc.SignatoryLinkID
import IPAddress
import MagicHash
import MinutesTime
import User.UserID
import Utils.Default

newtype SignOrder = SignOrder { unSignOrder :: Int32 }
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)

instance FromSQL SignOrder where
  type PQBase SignOrder = PQBase Int32
  fromSQL mbase = SignOrder <$> fromSQL mbase
instance ToSQL SignOrder where
  type PQDest SignOrder = PQDest Int32
  toSQL (SignOrder n) = toSQL n

---------------------------------

data SignInfo = SignInfo {
  signtime     :: !UTCTime
, signipnumber :: !IPAddress
} deriving (Eq, Ord, Show)

---------------------------------

data DeliveryStatus
  = Delivered
  | Undelivered
  | Unknown
  | Deferred
    deriving (Eq, Ord, Show)

instance PQFormat DeliveryStatus where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DeliveryStatus where
  type PQBase DeliveryStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Delivered
      2 -> return Undelivered
      3 -> return Unknown
      4 -> return Deferred
      _ -> throwM RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL DeliveryStatus where
  type PQDest DeliveryStatus = PQDest Int16
  toSQL Delivered   = toSQL (1::Int16)
  toSQL Undelivered = toSQL (2::Int16)
  toSQL Unknown     = toSQL (3::Int16)
  toSQL Deferred    = toSQL (4::Int16)

---------------------------------

data CSVUpload = CSVUpload {
  csvtitle     :: !String
, csvcontents  :: ![[String]]
} deriving (Eq, Ord, Show)

instance PQFormat [[String]] where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL [[String]] where
  type PQBase [[String]] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [[String]] where
  type PQDest [[String]] = PQDest String
  toSQL = jsonToSQL

---------------------------------

data AuthenticationMethod
  = StandardAuthentication
  | ELegAuthentication
  | SMSPinAuthentication
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL AuthenticationMethod where
  type PQBase AuthenticationMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return StandardAuthentication
      2 -> return ELegAuthentication
      3 -> return SMSPinAuthentication
      _ -> throwM RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL AuthenticationMethod where
  type PQDest AuthenticationMethod = PQDest Int16
  toSQL StandardAuthentication = toSQL (1::Int16)
  toSQL ELegAuthentication     = toSQL (2::Int16)
  toSQL SMSPinAuthentication   = toSQL (3::Int16)

---------------------------------

data DeliveryMethod
  = EmailDelivery
  | PadDelivery
  | APIDelivery
  | MobileDelivery
  | EmailAndMobileDelivery
    deriving (Eq, Ord, Show)

instance PQFormat DeliveryMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DeliveryMethod where
  type PQBase DeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EmailDelivery
      2 -> return PadDelivery
      3 -> return APIDelivery
      4 -> return MobileDelivery
      5 -> return EmailAndMobileDelivery
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL DeliveryMethod where
  type PQDest DeliveryMethod = PQDest Int16
  toSQL EmailDelivery          = toSQL (1::Int16)
  toSQL PadDelivery            = toSQL (2::Int16)
  toSQL APIDelivery            = toSQL (3::Int16)
  toSQL MobileDelivery         = toSQL (4::Int16)
  toSQL EmailAndMobileDelivery = toSQL (5::Int16)

---------------------------------

data ConfirmationDeliveryMethod
  = EmailConfirmationDelivery
  | MobileConfirmationDelivery
  | EmailAndMobileConfirmationDelivery
  | NoConfirmationDelivery
    deriving (Eq, Ord, Show)

instance PQFormat ConfirmationDeliveryMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL ConfirmationDeliveryMethod where
  type PQBase ConfirmationDeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EmailConfirmationDelivery
      2 -> return MobileConfirmationDelivery
      3 -> return EmailAndMobileConfirmationDelivery
      4 -> return NoConfirmationDelivery
      _ -> throwM RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL ConfirmationDeliveryMethod where
  type PQDest ConfirmationDeliveryMethod = PQDest Int16
  toSQL EmailConfirmationDelivery           = toSQL (1::Int16)
  toSQL MobileConfirmationDelivery          = toSQL (2::Int16)
  toSQL EmailAndMobileConfirmationDelivery  = toSQL (3::Int16)
  toSQL NoConfirmationDelivery              = toSQL (4::Int16)

---------------------------------

data SignatoryLink = SignatoryLink {
  signatorylinkid                         :: !SignatoryLinkID
, signatoryfields                         :: ![SignatoryField]
-- | True if signatory is an author of the document
, signatoryisauthor                       :: !Bool
-- | True if signatory participates in signing process
, signatoryispartner                      :: !Bool
, signatorysignorder                      :: !SignOrder
-- | Authentication code
, signatorymagichash                      :: !MagicHash
-- | If this document has been saved to an account, that is the user id
, maybesignatory                          :: !(Maybe UserID)
-- | When a person has signed this document
, maybesigninfo                           :: !(Maybe SignInfo)
-- | When a person has first seen this document
, maybeseeninfo                           :: !(Maybe SignInfo)
-- | when we receive confirmation that a user has read
, maybereadinvite                         :: !(Maybe UTCTime)
-- | Status of email delivery
, mailinvitationdeliverystatus            :: !DeliveryStatus
-- | Status of email delivery
, smsinvitationdeliverystatus             :: !DeliveryStatus
-- | When was put in recycle bin
, signatorylinkdeleted                    :: !(Maybe UTCTime)
-- | When was purged from the system
, signatorylinkreallydeleted              :: !(Maybe UTCTime)
, signatorylinkcsvupload                  :: !(Maybe CSVUpload)
, signatoryattachments                    :: ![SignatoryAttachment]
, signatorylinksignredirecturl            :: !(Maybe String)
, signatorylinkrejectredirecturl          :: !(Maybe String)
, signatorylinkrejectiontime              :: !(Maybe UTCTime)
, signatorylinkrejectionreason            :: !(Maybe String)
, signatorylinkauthenticationmethod       :: !AuthenticationMethod
, signatorylinkdeliverymethod             :: !DeliveryMethod
, signatorylinkconfirmationdeliverymethod :: !ConfirmationDeliveryMethod
} deriving (Show)

instance HasDefaultValue SignatoryLink where
  defaultValue = SignatoryLink {
    signatorylinkid = unsafeSignatoryLinkID 0
  , signatoryfields = []
  , signatoryisauthor = False
  , signatoryispartner = False
  , signatorysignorder = SignOrder 1
  , signatorymagichash = unsafeMagicHash 0
  , maybesignatory = Nothing
  , maybesigninfo = Nothing
  , maybeseeninfo = Nothing
  , maybereadinvite = Nothing
  , mailinvitationdeliverystatus = Unknown
  , smsinvitationdeliverystatus = Unknown
  , signatorylinkdeleted = Nothing
  , signatorylinkreallydeleted = Nothing
  , signatorylinkcsvupload = Nothing
  , signatoryattachments = []
  , signatorylinksignredirecturl = Nothing
  , signatorylinkrejectredirecturl = Nothing
  , signatorylinkrejectiontime = Nothing
  , signatorylinkrejectionreason = Nothing
  , signatorylinkauthenticationmethod = StandardAuthentication
  , signatorylinkdeliverymethod = EmailDelivery
  , signatorylinkconfirmationdeliverymethod = EmailConfirmationDelivery
  }

---------------------------------

signatoryLinksSelectors :: [SQL]
signatoryLinksSelectors = [
    "signatory_links.id"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryFieldsSelectors <> ")::signatory_field FROM signatory_link_fields WHERE signatory_links.id = signatory_link_fields.signatory_link_id ORDER BY signatory_link_fields.id)"
  , "signatory_links.is_author"
  , "signatory_links.is_partner"
  , "signatory_links.sign_order"
  , "signatory_links.token"
  , "signatory_links.user_id"
  , "signatory_links.sign_time"
  , "signatory_links.sign_ip"
  , "signatory_links.seen_time"
  , "signatory_links.seen_ip"
  , "signatory_links.read_invitation"
  , "signatory_links.mail_invitation_delivery_status"
  , "signatory_links.sms_invitation_delivery_status"
  , "signatory_links.deleted"
  , "signatory_links.really_deleted"
  , "signatory_links.csv_title"
  , "signatory_links.csv_contents"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryAttachmentsSelectors <> ")::signatory_attachment FROM signatory_attachments WHERE signatory_links.id = signatory_attachments.signatory_link_id ORDER BY signatory_attachments.file_id)"
  , "signatory_links.sign_redirect_url"
  , "signatory_links.reject_redirect_url"
  , "signatory_links.rejection_time"
  , "signatory_links.rejection_reason"
  , "signatory_links.authentication_method"
  , "signatory_links.delivery_method"
  , "signatory_links.confirmation_delivery_method"
  ]

type instance CompositeRow SignatoryLink = (SignatoryLinkID, CompositeArray1 SignatoryField, Bool, Bool, SignOrder, MagicHash, Maybe UserID, Maybe UTCTime, Maybe IPAddress, Maybe UTCTime, Maybe IPAddress, Maybe UTCTime, DeliveryStatus, DeliveryStatus, Maybe UTCTime, Maybe UTCTime, Maybe String, Maybe [[String]], CompositeArray1 SignatoryAttachment, Maybe String, Maybe String, Maybe UTCTime, Maybe String, AuthenticationMethod, DeliveryMethod, ConfirmationDeliveryMethod)

instance PQFormat SignatoryLink where
  pqFormat _ = "%signatory_link"

instance CompositeFromSQL SignatoryLink where
  toComposite (slid, CompositeArray1 fields, is_author, is_partner, sign_order, magic_hash, muser_id, msign_time, msign_ip, mseen_time, mseen_ip, mread_invite, mail_invitation_delivery_status, sms_invitation_delivery_status, mdeleted, mreally_deleted, mcsv_title, mcsv_contents, CompositeArray1 attachments, msign_redirect_url, mreject_redirect_url, mrejection_time, mrejection_reason, authentication_method, delivery_method, confirmation_delivery_method) = SignatoryLink {
    signatorylinkid = slid
  , signatoryfields = fields
  , signatoryisauthor = is_author
  , signatoryispartner = is_partner
  , signatorysignorder = sign_order
  , signatorymagichash = magic_hash
  , maybesignatory = muser_id
  , maybesigninfo = SignInfo <$> msign_time <*> msign_ip
  , maybeseeninfo = SignInfo <$> mseen_time <*> mseen_ip
  , maybereadinvite = mread_invite
  , mailinvitationdeliverystatus = mail_invitation_delivery_status
  , smsinvitationdeliverystatus = sms_invitation_delivery_status
  , signatorylinkdeleted = mdeleted
  , signatorylinkreallydeleted = mreally_deleted
  , signatorylinkcsvupload = CSVUpload <$> mcsv_title <*> mcsv_contents
  , signatoryattachments = attachments
  , signatorylinksignredirecturl = msign_redirect_url
  , signatorylinkrejectredirecturl = mreject_redirect_url
  , signatorylinkrejectiontime = mrejection_time
  , signatorylinkrejectionreason = mrejection_reason
  , signatorylinkauthenticationmethod = authentication_method
  , signatorylinkdeliverymethod = delivery_method
  , signatorylinkconfirmationdeliverymethod = confirmation_delivery_method
  }
