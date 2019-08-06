{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Types.SignatoryLink (
    SignOrder(..)
  , SignInfo(..)
  , DeliveryStatus(..)
  , CSVUpload(..)
  , AuthenticationKind(..)
  , AuthenticationToViewMethod(..)
  , AuthenticationToSignMethod(..)
  , DeliveryMethod(..)
  , ConfirmationDeliveryMethod(..)
  , NotificationDeliveryMethod(..)
  , ProcessFinishedAction(..)
  , SignatoryRole(..)
  , signatoryRoleFromBool
  , SignatoryLink(..)
  , signatoryLinksSelectors
  , defaultSignatoryLink
  , TemporaryMagicHash(..)
  , signatoryLinkMagicHashesSelectors
  , isValidSignatoryMagicHash
  ) where

import Control.Monad.Catch
import Data.Functor.Invariant
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes

import DB
import Doc.SignatoryLinkID
import Doc.Tables
import Doc.Types.HighlightedPage
import Doc.Types.SignatoryAttachment
import Doc.Types.SignatoryConsentQuestion
import Doc.Types.SignatoryField
import IPAddress
import MagicHash
import MinutesTime
import User.UserID
import Util.HasSomeUserInfo

data ProcessFinishedAction = DocumentSigned | DocumentApproved

newtype SignOrder = SignOrder { unSignOrder :: Int32 }
  deriving (Eq, Ord)
deriving newtype instance Read SignOrder
deriving newtype instance Show SignOrder

instance PQFormat SignOrder where
  pqFormat = pqFormat @Int32

instance Unjson SignOrder where
  unjsonDef = invmap SignOrder unSignOrder unjsonDef

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
  pqFormat = pqFormat @Int16

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
  csvcontents  :: ![[String]]
} deriving (Eq, Ord, Show)

instance PQFormat [[String]] where
  pqFormat = pqFormat @String
instance FromSQL [[String]] where
  type PQBase [[String]] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [[String]] where
  type PQDest [[String]] = PQDest String
  toSQL = jsonToSQL

---------------------------------

-- | Kind of an authentication to view.
data AuthenticationKind
  = AuthenticationToView
  | AuthenticationToViewArchived
    deriving (Eq, Ord, Show)

instance PQFormat AuthenticationKind where
  pqFormat = pqFormat @Int16

instance FromSQL AuthenticationKind where
  type PQBase AuthenticationKind = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return AuthenticationToView
      2 -> return AuthenticationToViewArchived
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL AuthenticationKind where
  type PQDest AuthenticationKind = PQDest Int16
  toSQL AuthenticationToView         = toSQL (1::Int16)
  toSQL AuthenticationToViewArchived = toSQL (2::Int16)

---------------------------------

data AuthenticationToViewMethod
  = StandardAuthenticationToView
  | SEBankIDAuthenticationToView
  | NOBankIDAuthenticationToView
  | DKNemIDAuthenticationToView
  | SMSPinAuthenticationToView
  | FITupasAuthenticationToView
  | VerimiAuthenticationToView
    deriving (Enum, Eq, Ord, Show)

instance PQFormat AuthenticationToViewMethod where
  pqFormat = pqFormat @Int16

instance FromSQL AuthenticationToViewMethod where
  type PQBase AuthenticationToViewMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return StandardAuthenticationToView
      2 -> return SEBankIDAuthenticationToView
      3 -> return NOBankIDAuthenticationToView
      4 -> return DKNemIDAuthenticationToView
      5 -> return SMSPinAuthenticationToView
      6 -> return FITupasAuthenticationToView
      7 -> return VerimiAuthenticationToView
      _ -> throwM RangeError {
        reRange = [(1, 7)]
      , reValue = n
      }

instance ToSQL AuthenticationToViewMethod where
  type PQDest AuthenticationToViewMethod = PQDest Int16
  toSQL StandardAuthenticationToView      = toSQL (1::Int16)
  toSQL SEBankIDAuthenticationToView      = toSQL (2::Int16)
  toSQL NOBankIDAuthenticationToView      = toSQL (3::Int16)
  toSQL DKNemIDAuthenticationToView       = toSQL (4::Int16)
  toSQL SMSPinAuthenticationToView        = toSQL (5::Int16)
  toSQL FITupasAuthenticationToView       = toSQL (6::Int16)
  toSQL VerimiAuthenticationToView        = toSQL (7::Int16)

---------------------------------

data AuthenticationToSignMethod
  = StandardAuthenticationToSign
  | SEBankIDAuthenticationToSign
  | SMSPinAuthenticationToSign
  | NOBankIDAuthenticationToSign
  | DKNemIDAuthenticationToSign
    deriving (Enum, Eq, Ord, Show)

instance PQFormat AuthenticationToSignMethod where
  pqFormat = pqFormat @Int16

instance FromSQL AuthenticationToSignMethod where
  type PQBase AuthenticationToSignMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return StandardAuthenticationToSign
      2 -> return SEBankIDAuthenticationToSign
      3 -> return SMSPinAuthenticationToSign
      4 -> return NOBankIDAuthenticationToSign
      5 -> return DKNemIDAuthenticationToSign
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL AuthenticationToSignMethod where
  type PQDest AuthenticationToSignMethod = PQDest Int16
  toSQL StandardAuthenticationToSign      = toSQL (1::Int16)
  toSQL SEBankIDAuthenticationToSign      = toSQL (2::Int16)
  toSQL SMSPinAuthenticationToSign        = toSQL (3::Int16)
  toSQL NOBankIDAuthenticationToSign      = toSQL (4::Int16)
  toSQL DKNemIDAuthenticationToSign       = toSQL (5::Int16)

---------------------------------

data DeliveryMethod
  = EmailDelivery
  | PadDelivery
  | APIDelivery
  | MobileDelivery
  | EmailAndMobileDelivery
    deriving (Eq, Ord, Show)

instance PQFormat DeliveryMethod where
  pqFormat = pqFormat @Int16

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
  | EmailLinkConfirmationDelivery
  | EmailLinkAndMobileConfirmationDelivery
    deriving (Eq, Ord, Show)

instance PQFormat ConfirmationDeliveryMethod where
  pqFormat = pqFormat @Int16

instance FromSQL ConfirmationDeliveryMethod where
  type PQBase ConfirmationDeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EmailConfirmationDelivery
      2 -> return MobileConfirmationDelivery
      3 -> return EmailAndMobileConfirmationDelivery
      4 -> return NoConfirmationDelivery
      5 -> return EmailLinkConfirmationDelivery
      6 -> return EmailLinkAndMobileConfirmationDelivery
      _ -> throwM RangeError {
        reRange = [(1, 6)]
      , reValue = n
      }

instance ToSQL ConfirmationDeliveryMethod where
  type PQDest ConfirmationDeliveryMethod = PQDest Int16
  toSQL EmailConfirmationDelivery              = toSQL (1::Int16)
  toSQL MobileConfirmationDelivery             = toSQL (2::Int16)
  toSQL EmailAndMobileConfirmationDelivery     = toSQL (3::Int16)
  toSQL NoConfirmationDelivery                 = toSQL (4::Int16)
  toSQL EmailLinkConfirmationDelivery          = toSQL (5::Int16)
  toSQL EmailLinkAndMobileConfirmationDelivery = toSQL (6::Int16)

data NotificationDeliveryMethod
  = EmailNotificationDelivery
  | MobileNotificationDelivery
  | EmailAndMobileNotificationDelivery
  | NoNotificationDelivery
    deriving (Eq, Ord, Show)

instance PQFormat NotificationDeliveryMethod where
  pqFormat = pqFormat @Int16

instance ToSQL NotificationDeliveryMethod where
  type PQDest NotificationDeliveryMethod = PQDest Int16
  toSQL NoNotificationDelivery                 = toSQL (0::Int16)
  toSQL EmailNotificationDelivery              = toSQL (1::Int16)
  toSQL MobileNotificationDelivery             = toSQL (2::Int16)
  toSQL EmailAndMobileNotificationDelivery     = toSQL (3::Int16)

instance FromSQL NotificationDeliveryMethod where
  type PQBase NotificationDeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      0 -> return NoNotificationDelivery
      1 -> return EmailNotificationDelivery
      2 -> return MobileNotificationDelivery
      3 -> return EmailAndMobileNotificationDelivery
      _ -> throwM RangeError {
        reRange = [(0, 3)]
      , reValue = n
      }

-- | Role through which the document is accessed.
data SignatoryRole = SignatoryRoleViewer
                   | SignatoryRoleSigningParty
                   | SignatoryRoleApprover
                   | SignatoryRoleForwardedSigningParty
                   | SignatoryRoleForwardedApprover
  deriving (Eq, Ord, Show)


-- | True == SigningParty, False == Viewer.
signatoryRoleFromBool :: Bool -> SignatoryRole
signatoryRoleFromBool signs = if signs then SignatoryRoleSigningParty
                              else SignatoryRoleViewer

instance PQFormat SignatoryRole where
  pqFormat = pqFormat @Int16

instance FromSQL SignatoryRole where
  type PQBase SignatoryRole = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SignatoryRoleViewer
      2 -> return SignatoryRoleSigningParty
      3 -> return SignatoryRoleApprover
      4 -> return SignatoryRoleForwardedSigningParty
      5 -> return SignatoryRoleForwardedApprover
      _ -> throwM RangeError {
        reRange = [(1,5)]
      , reValue = n
      }

instance ToSQL SignatoryRole where
  type PQDest SignatoryRole         = PQDest Int16
  toSQL SignatoryRoleViewer         = toSQL (1::Int16)
  toSQL SignatoryRoleSigningParty   = toSQL (2::Int16)
  toSQL SignatoryRoleApprover       = toSQL (3::Int16)
  toSQL SignatoryRoleForwardedSigningParty = toSQL (4::Int16)
  toSQL SignatoryRoleForwardedApprover     = toSQL (5::Int16)

---------------------------------

signatoryLinkMagicHashesSelectors :: [SQL]
signatoryLinkMagicHashesSelectors =
  [ "signatory_link_magic_hashes.hash"
  , "signatory_link_magic_hashes.expiration_time"
  ]

data TemporaryMagicHash = TemporaryMagicHash
  { temporaryMagicHashHash           :: MagicHash
  , temporaryMagicHashExpirationTime :: UTCTime
  } deriving (Eq, Show)

instance PQFormat TemporaryMagicHash where
  pqFormat = compositeTypePqFormat ctSignatoryLinkMagicHash

type instance CompositeRow TemporaryMagicHash = (MagicHash, UTCTime)

instance CompositeFromSQL TemporaryMagicHash where
  toComposite (hash, time) = TemporaryMagicHash hash time

-- | Check if the given magic hash is the permanent hash or one of the
-- temporary hashes. It relies on the assumption that the 'SignatoryLink'
-- contains only valid hashes. (See the WHERE statement in the query.)
isValidSignatoryMagicHash :: MagicHash -> SignatoryLink -> Bool
isValidSignatoryMagicHash mh sl =
  mh == signatorymagichash sl
  || mh `elem` map temporaryMagicHashHash (signatorytemporarymagichashes sl)

---------------------------------

data SignatoryLink = SignatoryLink {
  signatorylinkid                         :: !SignatoryLinkID
, signatoryfields                         :: ![SignatoryField]
-- | True if signatory is an author of the document
, signatoryisauthor                       :: !Bool
-- | Signatory role: viewer, signing party, approver
, signatoryrole                           :: !SignatoryRole
, signatorysignorder                      :: !SignOrder
-- | Authentication codes
, signatorymagichash                      :: !MagicHash
, signatorytemporarymagichashes           :: ![TemporaryMagicHash]
-- | If this document has been saved to an account, that is the user id
, maybesignatory                          :: !(Maybe UserID)
-- | When a person has signed this document (or approved, in case when
-- signatory role is approver).
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
, signatoryhighlightedpages               :: ![HighlightedPage]
, signatorylinksignredirecturl            :: !(Maybe String)
, signatorylinkrejectredirecturl          :: !(Maybe String)
, signatorylinkrejectiontime              :: !(Maybe UTCTime)
, signatorylinkrejectionreason            :: !(Maybe String)
, signatorylinkauthenticationtoviewmethod         :: !AuthenticationToViewMethod
, signatorylinkauthenticationtoviewarchivedmethod :: !AuthenticationToViewMethod
, signatorylinkauthenticationtosignmethod :: !AuthenticationToSignMethod
, signatorylinkdeliverymethod             :: !DeliveryMethod
, signatorylinkconfirmationdeliverymethod :: !ConfirmationDeliveryMethod
, signatorylinknotificationdeliverymethod :: !NotificationDeliveryMethod
, signatorylinkallowshighlighting         :: !Bool
-- | If a person has identified to view the document
, signatorylinkidentifiedtoview           :: !Bool
, signatorylinkhidepn                     :: !Bool
, signatorylinkcanbeforwarded             :: !Bool
-- | Consent module
, signatorylinkconsenttitle               :: !(Maybe String)
, signatorylinkconsentquestions           :: ![SignatoryConsentQuestion]
-- | Status of confirmation email delivery
, signatorylinkmailconfirmationdeliverystatus :: !DeliveryStatus
} deriving (Show)

defaultSignatoryLink :: SignatoryLink
defaultSignatoryLink =
  SignatoryLink {
    signatorylinkid = unsafeSignatoryLinkID 0
  , signatoryfields = []
  , signatoryisauthor = False
  , signatoryrole = SignatoryRoleViewer
  , signatorysignorder = SignOrder 1
  , signatorymagichash = unsafeMagicHash 0
  , signatorytemporarymagichashes = []
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
  , signatoryhighlightedpages = []
  , signatorylinksignredirecturl = Nothing
  , signatorylinkrejectredirecturl = Nothing
  , signatorylinkrejectiontime = Nothing
  , signatorylinkrejectionreason = Nothing
  , signatorylinkauthenticationtoviewmethod = StandardAuthenticationToView
  , signatorylinkauthenticationtoviewarchivedmethod = StandardAuthenticationToView
  , signatorylinkauthenticationtosignmethod = StandardAuthenticationToSign
  , signatorylinkdeliverymethod = EmailDelivery
  , signatorylinkconfirmationdeliverymethod = EmailConfirmationDelivery
  , signatorylinknotificationdeliverymethod = NoNotificationDelivery
  , signatorylinkallowshighlighting = False
  , signatorylinkidentifiedtoview = False
  , signatorylinkhidepn = False
  , signatorylinkcanbeforwarded = False
  , signatorylinkconsenttitle = Nothing
  , signatorylinkconsentquestions = []
  , signatorylinkmailconfirmationdeliverystatus = Unknown
  }

instance HasSomeUserInfo SignatoryLink where
  getEmail          = getEmail . signatoryfields
  getFirstName      = getFirstName . signatoryfields
  getLastName       = getLastName . signatoryfields
  getPersonalNumber = getPersonalNumber . signatoryfields
  getMobile         = getMobile . signatoryfields

---------------------------------

signatoryLinksSelectors :: [SQL]
signatoryLinksSelectors = [
    "signatory_links.id"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryFieldsSelectors <> ")::" <> raw (ctName ctSignatoryField) <+> "FROM signatory_link_fields WHERE signatory_links.id = signatory_link_fields.signatory_link_id ORDER BY signatory_link_fields.id)"
  , "documents.author_id = signatory_links.id"
  , "signatory_links.signatory_role"
  , "signatory_links.sign_order"
  , "signatory_links.token"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryLinkMagicHashesSelectors <> ")::" <> raw (ctName ctSignatoryLinkMagicHash) <+> "FROM signatory_link_magic_hashes WHERE signatory_links.id = signatory_link_magic_hashes.signatory_link_id AND signatory_link_magic_hashes.expiration_time > now())"
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
  , "signatory_links.csv_contents"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryAttachmentsSelectors <> ")::" <> raw (ctName ctSignatoryAttachment) <+> "FROM signatory_attachments LEFT JOIN files ON (files.id = signatory_attachments.file_id) WHERE signatory_links.id = signatory_attachments.signatory_link_id ORDER BY signatory_attachments.file_id, signatory_attachments.name)"
  , "ARRAY(SELECT (" <> mintercalate ", " highlightedPagesSelectors <> ")::" <> raw (ctName ctHighlightedPage) <+> "FROM highlighted_pages WHERE signatory_links.id = highlighted_pages.signatory_link_id ORDER BY highlighted_pages.id)"
  , "signatory_links.sign_redirect_url"
  , "signatory_links.reject_redirect_url"
  , "signatory_links.rejection_time"
  , "signatory_links.rejection_reason"
  , "signatory_links.authentication_to_view_method"
  , "signatory_links.authentication_to_view_archived_method"
  , "signatory_links.authentication_to_sign_method"
  , "signatory_links.delivery_method"
  , "signatory_links.confirmation_delivery_method"
  , "signatory_links.notification_delivery_method"
  , "signatory_links.allows_highlighting"
  , "(SELECT EXISTS (SELECT 1 FROM eid_authentications WHERE signatory_links.id = eid_authentications.signatory_link_id))"
  , "signatory_links.hide_pn_elog"
  , "signatory_links.can_be_forwarded"
  , "signatory_links.consent_title"
  , "ARRAY(SELECT (" <> mintercalate ", " signatoryConsentQuestionsSelectors <> ")::" <> raw (ctName ctSignatoryConsentQuestion) <+> "FROM signatory_link_consent_questions WHERE signatory_links.id = signatory_link_consent_questions.signatory_link_id ORDER BY position ASC)"
  , "signatory_links.mail_confirmation_delivery_status"
  ]

type instance CompositeRow SignatoryLink = (
    SignatoryLinkID
  , CompositeArray1 SignatoryField
  , Bool
  , SignatoryRole
  , SignOrder
  , MagicHash
  , CompositeArray1 TemporaryMagicHash
  , Maybe UserID
  , Maybe UTCTime
  , Maybe IPAddress
  , Maybe UTCTime
  , Maybe IPAddress
  , Maybe UTCTime
  , DeliveryStatus
  , DeliveryStatus
  , Maybe UTCTime
  , Maybe UTCTime
  , Maybe [[String]]
  , CompositeArray1 SignatoryAttachment
  , CompositeArray1 HighlightedPage
  , Maybe String
  , Maybe String
  , Maybe UTCTime
  , Maybe String
  , AuthenticationToViewMethod
  , AuthenticationToViewMethod
  , AuthenticationToSignMethod
  , DeliveryMethod
  , ConfirmationDeliveryMethod
  , NotificationDeliveryMethod
  , Bool
  , Bool
  , Bool
  , Bool
  , Maybe String
  , CompositeArray1 SignatoryConsentQuestion
  , DeliveryStatus)

instance PQFormat SignatoryLink where
  pqFormat = compositeTypePqFormat ctSignatoryLink

instance CompositeFromSQL SignatoryLink where
  toComposite ( slid
              , CompositeArray1 fields
              , is_author
              , signatory_role
              , sign_order
              , magic_hash
              , CompositeArray1 magic_hashes
              , muser_id
              , msign_time
              , msign_ip
              , mseen_time
              , mseen_ip
              , mread_invite
              , mail_invitation_delivery_status
              , sms_invitation_delivery_status
              , mdeleted
              , mreally_deleted
              , mcsv_contents
              , CompositeArray1 attachments
              , CompositeArray1 highlighted_pages
              , msign_redirect_url
              , mreject_redirect_url
              , mrejection_time
              , mrejection_reason
              , authentication_to_view_method
              , authentication_to_view_archived_method
              , authentication_to_sign_method
              , delivery_method
              , confirmation_delivery_method
              , notification_delivery_method
              , allows_highlighting
              , has_identified
              , hide_pn
              , canbeforwarded
              , consent_title
              , CompositeArray1 consent_questions
              , signatorylinkmailconfirmationdeliverystatus) = SignatoryLink {
    signatorylinkid = slid
  , signatoryfields = fields
  , signatoryisauthor = is_author
  , signatoryrole = signatory_role
  , signatorysignorder = sign_order
  , signatorymagichash = magic_hash
  , signatorytemporarymagichashes = magic_hashes
  , maybesignatory = muser_id
  , maybesigninfo = SignInfo <$> msign_time <*> msign_ip
  , maybeseeninfo = SignInfo <$> mseen_time <*> mseen_ip
  , maybereadinvite = mread_invite
  , mailinvitationdeliverystatus = mail_invitation_delivery_status
  , smsinvitationdeliverystatus = sms_invitation_delivery_status
  , signatorylinkdeleted = mdeleted
  , signatorylinkreallydeleted = mreally_deleted
  , signatorylinkcsvupload = CSVUpload <$> mcsv_contents
  , signatoryattachments = attachments
  , signatoryhighlightedpages = highlighted_pages
  , signatorylinksignredirecturl = msign_redirect_url
  , signatorylinkrejectredirecturl = mreject_redirect_url
  , signatorylinkrejectiontime = mrejection_time
  , signatorylinkrejectionreason = mrejection_reason
  , signatorylinkauthenticationtoviewmethod = authentication_to_view_method
  , signatorylinkauthenticationtoviewarchivedmethod = authentication_to_view_archived_method
  , signatorylinkauthenticationtosignmethod = authentication_to_sign_method
  , signatorylinkdeliverymethod = delivery_method
  , signatorylinkconfirmationdeliverymethod = confirmation_delivery_method
  , signatorylinknotificationdeliverymethod = notification_delivery_method
  , signatorylinkallowshighlighting = allows_highlighting
  , signatorylinkidentifiedtoview = has_identified
  , signatorylinkhidepn = hide_pn
  , signatorylinkcanbeforwarded = canbeforwarded
  , signatorylinkconsenttitle = consent_title
  , signatorylinkconsentquestions = consent_questions
  , signatorylinkmailconfirmationdeliverystatus
  }
