module Doc.Model where

import API.Service.Model
import DB.Derive
import DB.Types
import Doc.File
import User.Region
import User.UserID
import Company.Model
import MinutesTime

import Data.Data
import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as BS

-- newtypes
newtype AuthorAttachment = AuthorAttachment { authorattachmentfile :: File }
  deriving (Eq, Ord, Show)

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

newtype SignatoryAttachment = SignatoryAttachment { signatoryattachmentfile :: File }
  deriving (Eq, Ord, Show)

newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int64 }
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)

newtype SignOrder = SignOrder { unSignOrder :: Int32 }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)

-- enums
data CancelationReason = ManualCancel
                        -- The data returned by ELeg server
                        -- msg sl fn ln num
                        | ELegDataMismatch String SignatoryLinkID BS.ByteString BS.ByteString BS.ByteString
                          deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| CancelationReason |])

data DocumentFunctionality = BasicFunctionality | AdvancedFunctionality
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentFunctionality)

data DocumentLogEntry = DocumentLogEntry MinutesTime BS.ByteString
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [DocumentLogEntry] |])

data DocumentProcess = Contract | Offer | Order
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentProcess)

data DocumentStatus = Preparation
                    | Pending
                    | Closed
                    | Canceled
                    | Timedout
                    | Rejected
                    | AwaitingAuthor
                    | DocumentError String
                      deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| DocumentStatus |])

data DocumentSharing = Private | Shared
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentSharing)

data DocumentType = Signable | Template | Attachment | AttachmentTemplate
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentType)

data FieldType =
    FirstNameFT | LastNameFT | CompanyFT | PersonalNumberFT
  | CompanyNumberFT | EmailFT | CustomFT BS.ByteString Bool -- label filledbyauthor
    deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| FieldType |])

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
                          deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''IdentificationType)

data MailsDeliveryStatus = Delivered | Undelivered | Unknown | Deferred
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''MailsDeliveryStatus)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignatoryRole)

data SignatureProvider = BankIDProvider | TeliaProvider | NordeaProvider
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignatureProvider)

-- data structures
data CSVUpload = CSVUpload {
    csvtitle          :: BS.ByteString
  , csvcontents       :: [[BS.ByteString]]
  , csvsignatoryindex :: Int32
  } deriving (Eq, Ord, Show)

data DocumentTag = DocumentTag {
    tagname  :: BS.ByteString
  , tagvalue :: BS.ByteString
  } deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [DocumentTag] |])

data DocumentUI = DocumentUI {
    documentmailfooter :: Maybe BS.ByteString
  } deriving (Eq, Ord, Show)

data FieldPlacement = FieldPlacement {
    placementx          :: Int32
  , placementy          :: Int32
  , placementpage       :: Int32
  , placementpagewidth  :: Int32
  , placementpageheight :: Int32
  } deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryDetails = SignatoryDetails {
    signatorysignorder :: SignOrder
  , signatoryfields    :: [SignatoryField]
  } deriving (Eq, Ord, Show)

data SignatoryField = SignatoryField {
    sfType       :: FieldType
  , sfValue      :: BS.ByteString
  , sfPlacements :: [FieldPlacement]
  } deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [SignatoryField] |])

data SignatureInfo = SignatureInfo {
    signatureinfotext        :: String
  , signatureinfosignature   :: String
  , signatureinfocertificate :: String
  , signatureinfoprovider    :: SignatureProvider
  , signaturefstnameverified :: Bool
  , signaturelstnameverified :: Bool
  , signaturepersnumverified :: Bool
  } deriving (Eq, Ord, Show)

data SignInfo = SignInfo {
    signtime :: MinutesTime
  , signipnumber :: Word32
  } deriving (Eq, Ord, Show)

data SignatoryLink = SignatoryLink {
    signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
  , signatorydetails           :: SignatoryDetails    -- ^ details of this person as filled in invitation
  , signatorymagichash         :: MagicHash           -- ^ authentication code
  , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
  , maybecompany               :: Maybe CompanyID     -- ^ if this document has been saved to a company account this is the companyid
  , maybesigninfo              :: Maybe SignInfo      -- ^ when a person has signed this document
  , maybeseeninfo              :: Maybe SignInfo      -- ^ when a person has first seen this document
  , maybereadinvite            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
  , invitationdeliverystatus   :: MailsDeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatoryroles             :: [SignatoryRole]
  , signatorylinkdeleted       :: Bool -- ^ when true sends the doc to the recycle bin for that sig
  , signatorylinkreallydeleted :: Bool -- ^ when true it means that the doc has been removed from the recycle bin
  } deriving (Eq, Ord, Show)

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: BS.ByteString
  , documentsignatorylinks         :: [SignatoryLink]
  , documentfiles                  :: [File]
  , documentsealedfiles            :: [File]
  , documentstatus                 :: DocumentStatus
  , documentprocess                :: DocumentProcess
  , documenttype                   :: DocumentType
  , documentfunctionality          :: DocumentFunctionality
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Maybe Int
  , documenttimeouttime            :: Maybe MinutesTime
  , documentinvitetime             :: Maybe SignInfo
  , documentlog                    :: [DocumentLogEntry]  -- to be made into plain text
  , documentinvitetext             :: BS.ByteString
  , documenttrustweaverreference   :: Maybe BS.ByteString
  , documentallowedidtypes         :: [IdentificationType]
  , documentcsvupload              :: Maybe CSVUpload
  , documentcancelationreason      :: Maybe CancelationReason -- When a document iscancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
  , documentsharing                :: DocumentSharing
  , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
  , documenttags                   :: [DocumentTag]
  , documentservice                :: Maybe ServiceID
  , documentdeleted                :: Bool -- set to true when doc is deleted - the other fields will be cleared too, so it is really truely deleting, it's just we want to avoid re-using the docid.
  , documentauthorattachments      :: [AuthorAttachment]
  , documentsignatoryattachments   :: [SignatoryAttachment]
  , documentregion                 :: Region
  , documentui                     :: DocumentUI
  } deriving (Eq, Ord, Show)

